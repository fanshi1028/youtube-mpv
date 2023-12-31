// const port = chrome.runtime.connectNative("youtube-mpv");

chrome.runtime.onMessage.addListener((req, sender, res) => {
  const v = req.v;
  if (v) {
    chrome.runtime.sendNativeMessage("youtube.mpv", { v }, (r) => {
      if (chrome.runtime.lastError) {
        res(`ERROR: ${chrome.runtime.lastError.message}`);
      } else {
        res(`Messaging host: ${JSON.stringify(r)}`);
      }
    });
  } else res("no v");
  // NOTE: https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/runtime/onMessage#sending_an_asynchronous_response_using_sendresponse
  return true;
});
