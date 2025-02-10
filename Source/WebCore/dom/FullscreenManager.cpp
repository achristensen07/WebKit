/*
 * Copyright (C) 2019 Apple Inc. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE INC. AND ITS CONTRIBUTORS ``AS IS''
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL APPLE INC. OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "config.h"
#include "FullscreenManager.h"

#if ENABLE(FULLSCREEN_API)

#include "Chrome.h"
#include "ChromeClient.h"
#include "Document.h"
#include "DocumentInlines.h"
#include "Element.h"
#include "ElementInlines.h"
#include "EventLoop.h"
#include "EventNames.h"
#include "HTMLDialogElement.h"
#include "HTMLIFrameElement.h"
#include "HTMLMediaElement.h"
#include "HTMLNames.h"
#include "JSDOMPromiseDeferred.h"
#include "LocalDOMWindow.h"
#include "LocalFrame.h"
#include "Logging.h"
#include "Page.h"
#include "PseudoClassChangeInvalidation.h"
#include "QualifiedName.h"
#include "Quirks.h"
#include "RenderBlock.h"
#include "SVGElementTypeHelpers.h"
#include "SVGSVGElement.h"
#include "Settings.h"
#include <wtf/LoggerHelper.h>
#include <wtf/TZoneMallocInlines.h>

#if ENABLE(MATHML)
#include "MathMLMathElement.h"
#endif

namespace WebCore {

WTF_MAKE_TZONE_ALLOCATED_IMPL(FullscreenManager);

using namespace HTMLNames;

FullscreenManager::FullscreenManager(Page& page)
    : m_page(page)
{
}

FullscreenManager::~FullscreenManager() = default;

Element* FullscreenManager::fullscreenElement() const
{
    return m_fullscreenElement.get();
}

Document* FullscreenManager::mainFrameDocument()
{
    RefPtr page = m_page.get();
    if (!page)
        return nullptr;
    RefPtr mainFrame = m_page->localMainFrame();
    if (!mainFrame)
        return nullptr;
    return mainFrame->document();
}

RefPtr<Document> FullscreenManager::protectedMainFrameDocument()
{
    return mainFrameDocument();
}

// https://fullscreen.spec.whatwg.org/#dom-element-requestfullscreen
void FullscreenManager::requestFullscreenForElement(Ref<Element>&& element, RefPtr<DeferredPromise>&& promise, FullscreenCheckType checkType, CompletionHandler<void(bool)>&& completionHandler, HTMLMediaElementEnums::VideoFullscreenMode mode)
{
    enum class EmitErrorEvent : bool { No, Yes };
    auto handleError = [this, weakThis = WeakPtr { *this }](ASCIILiteral message, EmitErrorEvent emitErrorEvent, Ref<Element>&& element, RefPtr<DeferredPromise>&& promise, CompletionHandler<void(bool)>&& completionHandler) mutable {
        if (!weakThis)
            return;
        if (promise)
            promise->reject(Exception { ExceptionCode::TypeError, message });
        if (emitErrorEvent == EmitErrorEvent::Yes) {
            m_fullscreenErrorEventTargetQueue.append(element);
            element->document().scheduleRenderingUpdate(RenderingUpdateStep::Fullscreen);
        }
        completionHandler(false);
    };

    // If pendingDoc is not fully active, then reject promise with a TypeError exception and return promise.
    if (promise && !element->protectedDocument()->isFullyActive()) {
        handleError("Cannot request fullscreen on a document that is not fully active."_s, EmitErrorEvent::No, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    // https://fullscreen.spec.whatwg.org/#fullscreen-element-ready-check
    auto fullscreenElementReadyCheck = [checkType] (auto element, auto document) -> ASCIILiteral {
        if (!element->isConnected())
            return "Cannot request fullscreen on a disconnected element."_s;

        if (element->isPopoverShowing())
            return "Cannot request fullscreen on an open popover."_s;

        if (checkType == EnforceIFrameAllowFullscreenRequirement && !PermissionsPolicy::isFeatureEnabled(PermissionsPolicy::Feature::Fullscreen, document))
            return "Fullscreen API is disabled by permissions policy."_s;

        return { };
    };

    auto isElementTypeAllowedForFullscreen = [] (const auto& element) {
        if (is<HTMLElement>(element) || is<SVGSVGElement>(element))
            return true;
#if ENABLE(MATHML)
        if (is<MathMLMathElement>(element))
            return true;
#endif
        return false;
    };

    // If any of the following conditions are true, terminate these steps and queue a task to fire
    // an event named fullscreenerror with its bubbles attribute set to true on the context object's
    // node document:
    if (!isElementTypeAllowedForFullscreen(element)) {
        handleError("Cannot request fullscreen on a non-HTML element."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    if (is<HTMLDialogElement>(element)) {
        handleError("Cannot request fullscreen on a <dialog> element."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    if (auto error = fullscreenElementReadyCheck(element, element->protectedDocument())) {
        handleError(error, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    if (!element->document().domWindow() || !element->document().domWindow()->consumeTransientActivation()) {
        handleError("Cannot request fullscreen without transient activation."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    if (UserGestureIndicator::processingUserGesture() && UserGestureIndicator::currentUserGesture()->gestureType() == UserGestureType::EscapeKey) {
        handleError("Cannot request fullscreen with Escape key as current gesture."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    // There is a previously-established user preference, security risk, or platform limitation.
    if (!page() || !page()->isFullscreenManagerEnabled()) {
        handleError("Fullscreen API is disabled."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
        return;
    }

    bool hasKeyboardAccess = true;
    if (!page()->chrome().client().supportsFullScreenForElement(element, hasKeyboardAccess)) {
        // The new full screen API does not accept a "flags" parameter, so fall back to disallowing
        // keyboard input if the chrome client refuses to allow keyboard input.
        hasKeyboardAccess = false;

        if (!page()->chrome().client().supportsFullScreenForElement(element, hasKeyboardAccess)) {
            handleError("Cannot request fullscreen with unsupported element."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }
    }

    m_pendingFullscreenElement = RefPtr { element.ptr() };

    element->protectedDocument()->eventLoop().queueTask(TaskSource::MediaElement, [
        this,
        weakThis = WeakPtr { *this },
        element,
        promise = WTFMove(promise),
        completionHandler = WTFMove(completionHandler),
        hasKeyboardAccess,
        fullscreenElementReadyCheck,
        handleError,
        mode,
        initialDocument = WeakPtr { element->document() }
    ] () mutable {
        if (!weakThis) {
            if (promise)
                promise->reject(Exception { ExceptionCode::TypeError });
            completionHandler(false);
            return;
        }

        // Don't allow fullscreen if it has been cancelled or a different fullscreen element
        // has requested fullscreen.
        if (m_pendingFullscreenElement != element.ptr()) {
            handleError("Fullscreen request aborted by a fullscreen request for another element."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // Don't allow fullscreen if we're inside an exitFullscreen operation.
        if (m_pendingExitFullscreen) {
            handleError("Fullscreen request aborted by a request to exit fullscreen."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // Don't allow fullscreen if document is hidden.
        auto document = element->protectedDocument();
        if (document->hidden() && mode != HTMLMediaElementEnums::VideoFullscreenModeInWindow) {
            handleError("Cannot request fullscreen in a hidden document."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // Fullscreen element ready check.
        if (auto error = fullscreenElementReadyCheck(element, element->protectedDocument())) {
            handleError(error, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // Don't allow if element changed document.
        if (&element->document() != initialDocument.get()) {
            handleError("Cannot request fullscreen because the associated document has changed."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // A descendant browsing context's document has a non-empty fullscreen element stack.
        bool descendantHasNonEmptyStack = false;
        for (RefPtr descendant = document->frame() ? document->frame()->tree().traverseNext() : nullptr; descendant; descendant = descendant->tree().traverseNext()) {
            auto* localFrame = dynamicDowncast<LocalFrame>(descendant.get());
            if (!localFrame)
                continue;
            if (localFrame->document()->fullscreenManager()->fullscreenElement()) {
                descendantHasNonEmptyStack = true;
                break;
            }
        }
        if (descendantHasNonEmptyStack) {
            handleError("Cannot request fullscreen because a descendant document already has a fullscreen element."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
            return;
        }

        // 5. Return, and run the remaining steps asynchronously.
        // 6. Optionally, perform some animation.
        m_areKeysEnabledInFullscreen = hasKeyboardAccess;
        document->eventLoop().queueTask(TaskSource::MediaElement, [this, weakThis = WTFMove(weakThis), promise = WTFMove(promise), element = WTFMove(element), completionHandler = WTFMove(completionHandler), handleError = WTFMove(handleError), mode] () mutable {
            if (!weakThis) {
                if (promise)
                    promise->reject(Exception { ExceptionCode::TypeError });
                completionHandler(false);
                return;
            }

            RefPtr page = this->page();
            if (!page || (element->document().hidden() && mode != HTMLMediaElementEnums::VideoFullscreenModeInWindow) || m_pendingFullscreenElement != element.ptr() || !element->isConnected()) {
                handleError("Invalid state when requesting fullscreen."_s, EmitErrorEvent::Yes, WTFMove(element), WTFMove(promise), WTFMove(completionHandler));
                return;
            }

            // Reject previous promise, but continue with current operation.
            if (m_pendingPromise) {
                m_pendingPromise->reject(Exception { ExceptionCode::TypeError, "Pending operation cancelled by requestFullscreen() call."_s });
            }

            m_pendingPromise = WTFMove(promise);

            page->chrome().client().enterFullScreenForElement(element, mode);
            completionHandler(true);
        });

        // 7. Optionally, display a message indicating how the user can exit displaying the context object fullscreen.
    });
}

void FullscreenManager::cancelFullscreen()
{
    // The Mozilla "cancelFullscreen()" API behaves like the W3C "fully exit fullscreen" behavior, which
    // is defined as:
    // "To fully exit fullscreen act as if the exitFullscreen() method was invoked on the top-level browsing
    // context's document and subsequently empty that document's fullscreen element stack."
    RefPtr mainFrameDocument = this->mainFrameDocument();
    if (!mainFrameDocument)
        LOG_ONCE(SiteIsolation, "Unable to fully perform FullscreenManager::cancelFullscreen() without access to the main frame document ");

    if (!mainFrameDocument || !mainFrameDocument->fullscreenManager()->fullscreenElement()) {
        // If there is a pending fullscreen element but no top document fullscreen element,
        // there is a pending task in enterFullscreen(). Cause it to cancel and fire an error
        // by clearing the pending fullscreen element.
        m_pendingFullscreenElement = nullptr;
        if (m_pendingPromise) {
            m_pendingPromise->reject(Exception { ExceptionCode::TypeError, "Pending operation cancelled by webkitCancelFullScreen() call."_s });
            m_pendingPromise = nullptr;
        }
        return;
    }

    m_pendingExitFullscreen = true;

    if (!m_fullscreenElement)
        return;
    m_fullscreenElement->protectedDocument()->eventLoop().queueTask(TaskSource::MediaElement, [weakThis = WeakPtr { *this }, mainFrameDocument = WTFMove(mainFrameDocument)] {
#if RELEASE_LOG_DISABLED
        UNUSED_PARAM(this);
#endif
        if (!weakThis)
            return;

        if (!mainFrameDocument->page()) {
            return;
        }

        // This triggers finishExitFullscreen with ExitMode::Resize, which fully exits the document.
        if (RefPtr fullscreenElement = mainFrameDocument->fullscreenManager()->fullscreenElement())
            mainFrameDocument->page()->chrome().client().exitFullScreenForElement(fullscreenElement.get());
    });
}

// https://fullscreen.spec.whatwg.org/#collect-documents-to-unfullscreen
static Vector<Ref<Document>> documentsToUnfullscreen(Document& firstDocument)
{
    Vector<Ref<Document>> documents { Ref { firstDocument } };
    while (true) {
        auto lastDocument = documents.last();
        ASSERT(lastDocument->fullscreenManager()->fullscreenElement());
        if (!lastDocument->fullscreenManager()->isSimpleFullscreenDocument(lastDocument))
            break;
        auto frame = lastDocument->frame();
        if (!frame)
            break;
        auto frameOwner = frame->ownerElement();
        if (!frameOwner)
            break;
        if (auto* iframe = dynamicDowncast<HTMLIFrameElement>(frameOwner); iframe && iframe->hasIFrameFullscreenFlag())
            break;
        documents.append(frameOwner->document());
    }
    return documents;
}

static void clearFullscreenFlags(Element& element)
{
    element.setFullscreenFlag(false);
    if (auto* iframe = dynamicDowncast<HTMLIFrameElement>(element))
        iframe->setIFrameFullscreenFlag(false);
}

void FullscreenManager::exitFullscreen(Document& document, RefPtr<DeferredPromise>&& promise)
{
    Ref exitingDocument = document;
    auto mode = ExitMode::NoResize;
    auto exitDocuments = documentsToUnfullscreen(exitingDocument);

    RefPtr mainFrameDocument = this->mainFrameDocument();
    if (!mainFrameDocument)
        LOG_ONCE(SiteIsolation, "Unable to fully perform FullscreenManager::exitFullscreen() without access to the main frame document ");

    bool exitsTopDocument = exitDocuments.containsIf([&](auto& document) {
        return document.ptr() == mainFrameDocument.get();
    });
    if (exitsTopDocument && mainFrameDocument && mainFrameDocument->fullscreenManager()->isSimpleFullscreenDocument(document)) {
        mode = ExitMode::Resize;
        exitingDocument = *mainFrameDocument;
    }

    if (RefPtr element = exitingDocument->fullscreenManager()->fullscreenElement(); element && !element->isConnected()) {
        queueFullscreenChangeEventForDocument(exitingDocument);
        clearFullscreenFlags(*element);
        element->removeFromTopLayer();
    }

    m_pendingExitFullscreen = true;

    // Return promise, and run the remaining steps in parallel.
    exitingDocument->eventLoop().queueTask(TaskSource::MediaElement, [this, promise = WTFMove(promise), weakThis = WeakPtr { *this }, mode, exitingDocument] () mutable {
        if (!weakThis) {
            if (promise)
                promise->resolve();
            return;
        }

        RefPtr page = this->page();
        if (!page) {
            m_pendingExitFullscreen = false;
            if (promise)
                promise->resolve();
            return;
        }

        // If there is a pending fullscreen element but no fullscreen element
        // there is a pending task in requestFullscreenForElement(). Cause it to cancel and fire an error
        // by clearing the pending fullscreen element.
        if (!m_fullscreenElement && m_pendingFullscreenElement) {
            m_pendingFullscreenElement = nullptr;
            m_pendingExitFullscreen = false;
            if (promise)
                promise->resolve();
            return;
        }

        if (m_pendingPromise)
            m_pendingPromise->reject(Exception { ExceptionCode::TypeError, "Pending operation cancelled by exitFullscreen() call."_s });

        m_pendingPromise = WTFMove(promise);

        // Notify the chrome of the new full screen element.
        if (mode == ExitMode::Resize)
            page->chrome().client().exitFullScreenForElement(m_fullscreenElement.get());
        else {
            finishExitFullscreen(exitingDocument, ExitMode::NoResize);

            m_pendingFullscreenElement = fullscreenElement();
            if (m_pendingFullscreenElement)
                page->chrome().client().enterFullScreenForElement(*m_pendingFullscreenElement);
            else
                resolvePendingPromise();
        }
    });
}

void FullscreenManager::finishExitFullscreen(Document& currentDocument, ExitMode mode)
{
    if (!currentDocument.fullscreenManager()->fullscreenElement())
        return;

    // Let descendantDocs be an ordered set consisting of doc’s descendant browsing contexts' active documents whose fullscreen element is non-null, if any, in tree order.
    Deque<Ref<Document>> descendantDocuments;
    for (RefPtr descendant = currentDocument.frame() ? currentDocument.frame()->tree().traverseNext() : nullptr; descendant; descendant = descendant->tree().traverseNext()) {
        RefPtr localFrame = dynamicDowncast<LocalFrame>(descendant);
        if (!localFrame || !localFrame->document())
            continue;
        if (localFrame->document()->fullscreenManager()->fullscreenElement())
            descendantDocuments.prepend(*localFrame->document());
    }

    auto unfullscreenDocument = [](const Ref<Document>& document) {
        Vector<Ref<Element>> toRemove;
        for (Ref element : document->topLayerElements()) {
            if (!element->hasFullscreenFlag())
                continue;
            clearFullscreenFlags(element);
            toRemove.append(element);
        }
        for (Ref element : toRemove)
            element->removeFromTopLayer();
    };

    auto exitDocuments = documentsToUnfullscreen(currentDocument);
    for (Ref exitDocument : exitDocuments) {
        queueFullscreenChangeEventForDocument(exitDocument);
        if (mode == ExitMode::Resize)
            unfullscreenDocument(exitDocument);
        else {
            RefPtr fullscreenElement = exitDocument->fullscreenManager()->fullscreenElement();
            clearFullscreenFlags(*fullscreenElement);
            fullscreenElement->removeFromTopLayer();
        }
    }

    for (Ref descendantDocument : descendantDocuments) {
        queueFullscreenChangeEventForDocument(descendantDocument);
        unfullscreenDocument(descendantDocument);
    }
}

bool FullscreenManager::isFullscreenEnabled(Document& document) const
{
    // 4. The fullscreenEnabled attribute must return true if the context object and all ancestor
    // browsing context's documents have their fullscreen enabled flag set, or false otherwise.

    // Top-level browsing contexts are implied to have their allowFullscreen attribute set.
    return PermissionsPolicy::isFeatureEnabled(PermissionsPolicy::Feature::Fullscreen, document);
}

bool FullscreenManager::willEnterFullscreen(Element& element, HTMLMediaElementEnums::VideoFullscreenMode mode)
{
#if !ENABLE(VIDEO)
    UNUSED_PARAM(mode);
#endif

    if (element.document().backForwardCacheState() != Document::NotInBackForwardCache) {
        rejectPendingPromise(Exception { ExceptionCode::TypeError });
        return false;
    }

    // Protect against being called after the document has been removed from the page.
    RefPtr protectedPage = page();
    if (!protectedPage) {
        rejectPendingPromise(Exception { ExceptionCode::TypeError });
        return false;
    }

    // The element is an open popover.
    if (element.isPopoverShowing()) {
        rejectPendingPromise(Exception { ExceptionCode::TypeError, "Cannot request fullscreen on an open popover."_s });
        return false;
    }

    // If pending fullscreen element is unset or another element's was requested,
    // issue a cancel fullscreen request to the client
    if (m_pendingFullscreenElement != &element) {
        page()->chrome().client().exitFullScreenForElement(&element);
        rejectPendingPromise(Exception { ExceptionCode::TypeError, "Element requested for fullscreen has changed."_s });
        return false;
    }

    ASSERT(page()->isFullscreenManagerEnabled());

#if ENABLE(VIDEO)
    if (RefPtr mediaElement = dynamicDowncast<HTMLMediaElement>(element))
        mediaElement->willBecomeFullscreenElement(mode);
    else
#endif
        element.willBecomeFullscreenElement();

    ASSERT(&element == m_pendingFullscreenElement);
    m_pendingFullscreenElement = nullptr;

    m_fullscreenElement = &element;

    Deque<RefPtr<Element>> ancestors;
    RefPtr ancestor = &element;
    do {
        ancestors.append(ancestor);
    } while ((ancestor = ancestor->document().ownerElement()));

    for (auto ancestor : ancestors) {
        auto hideUntil = ancestor->topmostPopoverAncestor(Element::TopLayerElementType::Other);
        ancestor->document().hideAllPopoversUntil(hideUntil, FocusPreviousElement::No, FireEvents::No);

        auto containingBlockBeforeStyleResolution = SingleThreadWeakPtr<RenderBlock> { };
        if (auto* renderer = ancestor->renderer())
            containingBlockBeforeStyleResolution = renderer->containingBlock();

        ancestor->setFullscreenFlag(true);
        ancestor->document().resolveStyle(Document::ResolveStyleType::Rebuild);

        // Remove before adding, so we always add at the end of the top layer.
        if (ancestor->isInTopLayer())
            ancestor->removeFromTopLayer();
        ancestor->addToTopLayer();

        queueFullscreenChangeEventForDocument(ancestor->document());

        RenderElement::markRendererDirtyAfterTopLayerChange(ancestor->checkedRenderer().get(), containingBlockBeforeStyleResolution.get());
    }

    if (auto* iframe = dynamicDowncast<HTMLIFrameElement>(element))
        iframe->setIFrameFullscreenFlag(true);

    resolvePendingPromise();
    return true;
}

bool FullscreenManager::didEnterFullscreen()
{
    RefPtr fullscreenElement = this->fullscreenElement();
    if (!fullscreenElement) {
        return false;
    }

    if (fullscreenElement->document().backForwardCacheState() != Document::NotInBackForwardCache) {
        return false;
    }

    fullscreenElement->didBecomeFullscreenElement();
    return true;
}

bool FullscreenManager::willExitFullscreen()
{
    auto fullscreenElement = fullscreenOrPendingElement();
    if (!fullscreenElement) {
        return false;
    }

    if (fullscreenElement->document().backForwardCacheState() != Document::NotInBackForwardCache) {
        return false;
    }

    fullscreenElement->willStopBeingFullscreenElement();
    return true;
}

bool FullscreenManager::didExitFullscreen()
{
    auto fullscreenElement = fullscreenOrPendingElement();
    if (!fullscreenElement) {
        m_pendingExitFullscreen = false;
        rejectPendingPromise(Exception { ExceptionCode::TypeError, "No fullscreen element to exit."_s });
        return false;
    }

    if (fullscreenElement->document().backForwardCacheState() != Document::NotInBackForwardCache) {
        m_pendingExitFullscreen = false;
        rejectPendingPromise(Exception { ExceptionCode::TypeError });
        return false;
    }

    if (RefPtr mainFrameDocument = protectedMainFrameDocument())
        finishExitFullscreen(*mainFrameDocument, ExitMode::Resize);
    else
        LOG_ONCE(SiteIsolation, "Unable to fully perform FullscreenManager::didExitFullscreen() without access to the main frame document ");

    if (fullscreenElement)
        fullscreenElement->didStopBeingFullscreenElement();

    m_areKeysEnabledInFullscreen = false;

    m_fullscreenElement = nullptr;
    m_pendingFullscreenElement = nullptr;
    m_pendingExitFullscreen = false;

    resolvePendingPromise();
    return true;
}

void FullscreenManager::resolvePendingPromise()
{
    if (!m_pendingPromise)
        return;
    m_pendingPromise->resolve();
    m_pendingPromise = nullptr;
}

void FullscreenManager::rejectPendingPromise(Exception exception)
{
    if (!m_pendingPromise)
        return;
    m_pendingPromise->reject(WTFMove(exception));
    m_pendingPromise = nullptr;
}

// https://fullscreen.spec.whatwg.org/#run-the-fullscreen-steps
void FullscreenManager::dispatchPendingEvents(Document& document)
{
    // Since we dispatch events in this function, it's possible that the
    // document will be detached and GC'd. We protect it here to make sure we
    // can finish the function successfully.
    Deque<GCReachableRef<Node>> changeQueue;
    m_fullscreenChangeEventTargetQueue.swap(changeQueue);
    Deque<GCReachableRef<Node>> errorQueue;
    m_fullscreenErrorEventTargetQueue.swap(errorQueue);

    dispatchFullscreenChangeOrErrorEvent(document, changeQueue, EventType::Change, /* shouldNotifyMediaElement */ true);
    dispatchFullscreenChangeOrErrorEvent(document, errorQueue, EventType::Error, /* shouldNotifyMediaElement */ false);
}

void FullscreenManager::dispatchEventForNode(Node& node, EventType eventType)
{
    switch (eventType) {
    case EventType::Change: {
        node.dispatchEvent(Event::create(eventNames().fullscreenchangeEvent, Event::CanBubble::Yes, Event::IsCancelable::No, Event::IsComposed::Yes));
        bool shouldEmitUnprefixed = !(node.hasEventListeners(eventNames().webkitfullscreenchangeEvent) && node.hasEventListeners(eventNames().fullscreenchangeEvent)) && !(node.document().hasEventListeners(eventNames().webkitfullscreenchangeEvent) && node.document().hasEventListeners(eventNames().fullscreenchangeEvent));
        if (shouldEmitUnprefixed)
            node.dispatchEvent(Event::create(eventNames().webkitfullscreenchangeEvent, Event::CanBubble::Yes, Event::IsCancelable::No, Event::IsComposed::Yes));
        break;
    }
    case EventType::Error:
        node.dispatchEvent(Event::create(eventNames().fullscreenerrorEvent, Event::CanBubble::Yes, Event::IsCancelable::No, Event::IsComposed::Yes));
        node.dispatchEvent(Event::create(eventNames().webkitfullscreenerrorEvent, Event::CanBubble::Yes, Event::IsCancelable::No, Event::IsComposed::Yes));
        break;
    }
}

void FullscreenManager::dispatchFullscreenChangeOrErrorEvent(Document& document, Deque<GCReachableRef<Node>>& queue, EventType eventType, bool shouldNotifyMediaElement)
{
    // Step 3 of https://fullscreen.spec.whatwg.org/#run-the-fullscreen-steps
    while (!queue.isEmpty()) {
        auto node = queue.takeFirst();

        // Gaining or losing fullscreen state may change viewport arguments
        node->protectedDocument()->updateViewportArguments();

#if ENABLE(VIDEO)
        if (shouldNotifyMediaElement) {
            if (RefPtr mediaElement = dynamicDowncast<HTMLMediaElement>(node.get()))
                mediaElement->enteredOrExitedFullscreen();
        }
#else
        UNUSED_PARAM(shouldNotifyMediaElement);
#endif
        // If the element was removed from our tree, also message the documentElement. Since we may
        // have a document hierarchy, check that node isn't in another document.
        if (!node->isConnected() || &node->document() != &document)
            queue.append(document);
        else
            dispatchEventForNode(node.get(), eventType);
    }
}

void FullscreenManager::exitRemovedFullscreenElement(Element& element)
{
    ASSERT(element.hasFullscreenFlag());

    auto fullscreenElement = fullscreenOrPendingElement();
    if (fullscreenElement == &element) {
        exitFullscreen(element.document(), nullptr);
    } else
        clearFullscreenFlags(element);
}

bool FullscreenManager::isAnimatingFullscreen() const
{
    return m_isAnimatingFullscreen;
}

void FullscreenManager::setAnimatingFullscreen(bool flag)
{
    if (m_isAnimatingFullscreen == flag)
        return;

    std::optional<Style::PseudoClassChangeInvalidation> styleInvalidation;
    if (RefPtr fullscreenElement = this->fullscreenElement())
        emplace(styleInvalidation, *fullscreenElement, { { CSSSelector::PseudoClass::InternalAnimatingFullscreenTransition, flag } });
    m_isAnimatingFullscreen = flag;
}

void FullscreenManager::clear()
{
    m_fullscreenElement = nullptr;
    m_pendingFullscreenElement = nullptr;
    m_pendingPromise = nullptr;
}

void FullscreenManager::emptyEventQueue()
{
    m_fullscreenChangeEventTargetQueue.clear();
    m_fullscreenErrorEventTargetQueue.clear();
}

void FullscreenManager::queueFullscreenChangeEventForDocument(Document& document)
{
    RefPtr target = document.fullscreenManager()->fullscreenElement();
    if (!target) {
        ASSERT_NOT_REACHED();
        return;
    }
    document.fullscreenManager()->addElementToChangeEventQueue(*target);
    document.scheduleRenderingUpdate(RenderingUpdateStep::Fullscreen);
}

bool FullscreenManager::isSimpleFullscreenDocument(Document& document) const
{
    bool foundFullscreenFlag = false;
    for (Ref element : document.topLayerElements()) {
        if (element->hasFullscreenFlag()) {
            if (foundFullscreenFlag)
                return false;
            foundFullscreenFlag = true;
        }
    }
    return foundFullscreenFlag;
}

bool FullscreenManager::fullscreenElementIsInTopDocument() const
{
    if (!m_fullscreenElement)
        return false;
    return m_fullscreenElement->document().isTopDocument();
}

}

#endif
