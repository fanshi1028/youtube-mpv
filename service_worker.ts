// const port = chrome.runtime.connectNative("youtube-mpv");

chrome.runtime.onMessage.addListener((req, sender, res) => {
  const v = req.v;
  if (v) {
    chrome.runtime.sendNativeMessage("youtube.mpv", { v }, (r) => {
      if (chrome.runtime.lastError) {
        console.log(`ERROR: ${chrome.runtime.lastError.message}`);
        res(`ERROR: ${chrome.runtime.lastError.message}`);
      } else {
        console.log(`Messaging host: ${r}`);
        res(`Messaging host: ${r}`);
      }
    });
  } else res("no v");
});
