(function () {
  var version = "mx-black-20260422-1";
  var selectors = [
    'link[rel="icon"]',
    'link[rel="apple-touch-icon"]',
    'link[rel="manifest"]'
  ];

  function withVersion(href) {
    if (!href) return href;
    if (href.indexOf("data:") === 0) return href;
    var separator = href.indexOf("?") === -1 ? "?" : "&";
    return href + separator + "v=" + version;
  }

  function refreshFavicons() {
    var head = document.head;
    var links = document.querySelectorAll(selectors.join(","));

    links.forEach(function (link) {
      var clone = link.cloneNode(true);
      clone.setAttribute("href", withVersion(link.getAttribute("href")));
      head.removeChild(link);
      head.appendChild(clone);
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", refreshFavicons, { once: true });
  } else {
    refreshFavicons();
  }
})();
