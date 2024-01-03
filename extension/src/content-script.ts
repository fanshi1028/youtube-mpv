// create observer
const observer = new MutationObserver((mutationList, _observer) => {
  for (const mutation of mutationList) {
    if (mutation.type === "childList") {
      for (const node of mutation.addedNodes) {
        if (
          node.nodeType == 1 &&
          node instanceof Element &&
          node.tagName == "YTD-RICH-GRID-MEDIA"
        ) {
          const dismissible = node.firstElementChild;
          const thumbnail = dismissible?.firstElementChild;
          if (thumbnail) {
            dismissible.addEventListener(
              "mouseenter",
              () => {
                const a_thumbnail = thumbnail.querySelector("a#thumbnail");

                const match = /^\/watch\?v=(.+)$/g.exec(
                  a_thumbnail?.getAttribute("href") || "",
                );
                const v = match && match[1].split("&")[0];
                const img = a_thumbnail?.querySelector("img");

                if (img && v) {
                  img.addEventListener("click", () =>
                    chrome.runtime.sendMessage({ v }, (res) =>
                      console.log(res),
                    ),
                  );
                  dismissible.replaceChild(img, thumbnail);
                } else console.log(`fuck, img: ${img}, v: ${v}`);
              },
              { once: true },
            );
          }
        }
      }
    }
  }
});

observer.observe(document, { childList: true, subtree: true });
