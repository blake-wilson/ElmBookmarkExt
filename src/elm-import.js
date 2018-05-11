import {MDCIconToggle} from '@material/icon-toggle';
import html2canvas from 'html2canvas';

document.addEventListener('DOMContentLoaded', function() {
    var div = document.getElementById('main');
    const app = Elm.Main.embed(div);
    app.ports.getBookmarks.subscribe(function() {
      var tree = chrome.bookmarks.getTree(function(arr) {
        console.log('sending bookmarks ');
        app.ports.handleBookmarks.send(arr[0]);
      });
    });
    app.ports.reRender.subscribe(function() {
      requestAnimationFrame( () => {
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
    });
    // Initialize bookmarks
    var tree = chrome.bookmarks.getTree(function(arr) {
      app.ports.handleBookmarks.send(arr[0]);
    });
  });
