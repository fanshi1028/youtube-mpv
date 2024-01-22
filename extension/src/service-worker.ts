let mpvPort = chrome.runtime.connectNative("youtube.mpv");

mpvPort.postMessage({ command: ["request_log_messages"] });

// NOTE: Keep mpv idle intead of getting closed when q is pressed
mpvPort.postMessage({ command: ["keybind", "q", "stop"] });

mpvPort.onDisconnect.addListener((_) => console.log("disconnected"));

mpvPort.onMessage.addListener((r) => {
  if (chrome.runtime.lastError) {
    console.log(
      `ERROR: ${
        chrome.runtime.lastError.message ||
        "Got error but chrome.runtime.lastError.message is null"
      }`,
    );
  } else {
    let cms;
    try {
      cms = JSON.parse(r);
    } catch (e) {}
    if (cms && cms instanceof Array) {
      cms.forEach((cm: YoutubeComment) =>
        console.log(`comment: ${cm.text}\n${cm.like_count}`),
      );
    } else console.log(`mpv: ${JSON.stringify(r)}`);
  }
});

chrome.runtime.onMessage.addListener((msg, _sender, send) => {
  if (msg.v) {
    mpvPort.postMessage({ command: ["loadfile", `ytdl://${msg.v}`] });
    send(`loading ${msg.v}`);
  } else {
    send("Message has no v");
  }
});

const mpvGetProp = (prop: string) =>
  mpvPort.postMessage({ command: ["get_property", prop] });

const mpvSetProp = (prop: string, value: string) =>
  mpvPort.postMessage({ command: ["set_property", prop, value] });
