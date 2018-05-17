chrome.browserAction.onClicked.addListener(function(tab) {
  chrome.windows.create({
    url: chrome.runtime.getURL("index.html"),
    height: 800,
    width: 1000
  });
});
