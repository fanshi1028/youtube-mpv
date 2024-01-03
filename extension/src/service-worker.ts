let mpvPort = chrome.runtime.connectNative("youtube.mpv");

// mpvPort.postMessage({ command: ["request_log_messages"] });

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
    console.log(`Messaging host: ${JSON.stringify(r)}`);
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
