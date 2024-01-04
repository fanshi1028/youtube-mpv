// create observer
const observer = new MutationObserver((mutationList, _observer) => {
  for (const mutation of mutationList) {
    if (mutation.type === "childList") {
      for (const node of mutation.addedNodes) {
        if (
          node.nodeType == 1 &&
          node instanceof Element &&
          (node.tagName == "YTD-RICH-GRID-MEDIA" ||
            node.tagName == "YTD-VIDEO-RENDERER")
        ) {
          const dismissible = node.firstElementChild;
          const thumbnail = dismissible?.firstElementChild;
          if (thumbnail) {
            dismissible.addEventListener(
              "mouseenter",
              () => {
                const a_thumbnail = thumbnail.querySelector("a#thumbnail");

                if (a_thumbnail) {
                  const match = /^\/watch\?v=(.+)$/g.exec(
                    a_thumbnail.getAttribute("href") || "",
                  );
                  const v = match && match[1].split("&")[0];
                  const img = a_thumbnail.querySelector("img");

                  if (img && v) {
                    img.classList.add("ytd-thumbnail");
                    img.style.borderRadius = "12px";
                    if (node.tagName == "YTD-VIDEO-RENDERER") {
                      img.style.marginRight = "16px";
                      img.style.maxWidth = "360px";
                      img.style.minWidth = "240px";
                    }
                    img.addEventListener("click", () =>
                      chrome.runtime.sendMessage({ v }, (res) =>
                        console.log(res),
                      ),
                    );
                    dismissible.replaceChild(img, thumbnail);
                  } else console.log(`fuck, img: ${img}, v: ${v}`);
                } else console.log(`fuck, a_thumbnail: ${a_thumbnail}`);
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
