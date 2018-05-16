import {MDCIconToggle} from '@material/icon-toggle';
import {MDCRipple} from '@material/ripple';
import html2canvas from 'html2canvas';
import { Z_DEFAULT_STRATEGY } from 'zlib';

const storageKey = 'bookmarkLinks';

document.addEventListener('DOMContentLoaded', function() {
    var div = document.getElementById('main');
    const app = Elm.Main.embed(div);
    app.ports.getBookmarks.subscribe(function() {
      var tree = chrome.bookmarks.getTree(function(arr) {
        console.log('sending bookmarks ');
        // assignLinks(arr[0]);
        app.ports.handleBookmarks.send(arr[0]);
      });
    });
    app.ports.reRender.subscribe(function() {
      requestAnimationFrame( () => {
        document.querySelectorAll('.ripple-btn').forEach(function(el) {
          MDCRipple.attachTo(el);
        });
        document.querySelectorAll('.toggle-btn').forEach(function(el) {
          MDCIconToggle.attachTo(el);
          el.addEventListener('MDCIconToggle:change', ({detail}) => {
            var nodeId = el.getAttribute('node-id');
            app.ports.toggleExpand.send(nodeId);
          });
        });
      });
    })
    app.ports.openTab.subscribe(function(url) {
        console.log('opened tab at ', url);
        chrome.tabs.create({ url: url });
    });
    app.ports.backup.subscribe(function(idAndLink) {
      var id = idAndLink[0];
      var link = idAndLink[1];
      console.log('id: ' + id + ' and link ' + link);
      chrome.storage.local.get(storageKey, function(bookmarkLinks) {
        if (bookmarkLinks[storageKey] === undefined) {
          bookmarkLinks[storageKey] = {}
        }
        bookmarkLinks[storageKey][id] = link;
        chrome.storage.local.set(bookmarkLinks, function() {
          console.log('Bookmarks are now: ', bookmarkLinks);
        });
        app.ports.handleLinks.send({[id]: link});
      })
    });
    // Initialize bookmarks
    var tree = chrome.bookmarks.getTree(function(arr) {
      chrome.storage.local.get(storageKey, function(bookmarkLinks) {
        if (bookmarkLinks[storageKey] === undefined) {
          bookmarkLinks[storageKey] = {}
        }
        assignLinks(arr[0], bookmarkLinks[storageKey]);
        app.ports.handleBookmarks.send(arr[0]);
      });
    });
});

function assignLinks(bookmark, bookmarkLinks) {
  if (bookmark.children) {
    bookmark.children.forEach(function(el) {
      assignLinks(el, bookmarkLinks)
    });
  }
  if (bookmark.url) {
    if (bookmarkLinks[bookmark.id] !== undefined) {
      bookmark.backupLink = bookmarkLinks[bookmark.id];
    }
  }
}
