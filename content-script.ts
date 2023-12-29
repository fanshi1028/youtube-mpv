// create observer
const observer = new MutationObserver((mutationList, _observer) => {
    for (const mutation of mutationList) {
        if (mutation.type === "childList") {
            for (const node of mutation.addedNodes) {
                if (node.nodeType == 1 && node instanceof Element && node.tagName == "YTD-RICH-GRID-MEDIA") {
                    const dismissible = node.firstElementChild
                    const thumbnail = dismissible?.firstElementChild
                    if (thumbnail) {
                        dismissible.addEventListener("mouseenter", () => {

                            const a_thumbnail = thumbnail.querySelector("a#thumbnail")
                            const match = /^\/watch\?v=(.+)$/g.exec(a_thumbnail?.getAttribute("href") || "")
                            const v = match && match[1].split("&")[0]

                            const img = a_thumbnail?.querySelector("img")

                            if (img) {
                                img.addEventListener("click", () => {
                                    console.log(v)
                                    chrome.runtime.sendMessage({ v }).then(res => console.log(res))
                                })
                                dismissible.replaceChild(img, thumbnail)
                            } else console.log("fuck")

                        }, { once: true })
                    }
                }
            }
        }
    }
    // if (found.length > 0)
    //     observer.disconnect()
});

observer.observe(document, { childList: true, subtree: true });

// const observer = new MutationObserver((mutationList, _observer) => {
//     for (const mutation of mutationList) {
//         if (mutation.type === "childList") {
//             for (const node of mutation.addedNodes) {
//                 if (node.nodeType == 1 && node instanceof Element && node.tagName == "A") {
//                     const match = /^\/watch\?v=(.+)$/g.exec(node.getAttribute("href") || "")
//                     const v = match && match[1].split("&")[0]
//                     if (v) {
//                         node.removeAttribute("href")
//                         node.addEventListener("click", () => {
//                             console.log(v)
//                             chrome.runtime.sendMessage({ v }).then(res => console.log(res))
//                         })
//                     }
//                 }
//             }
//         }
//     }
//     // if (found.length > 0)
//     //     observer.disconnect()
// });
