// Move the auto-generated dark-mode toggle so it sits before the
// social icons on the navbar instead of after them.
document.addEventListener("DOMContentLoaded", function () {
  var toggle = document.querySelector(".quarto-color-scheme-toggle");
  var iconsList = document.querySelector(".navbar-nav.navbar-nav-scroll.ms-auto");
  var toolsContainer = document.querySelector(".quarto-navbar-tools");

  if (!toggle || !iconsList) {
    return;
  }

  var li = document.createElement("li");
  li.className = "nav-item quarto-color-scheme-toggle-item";
  toggle.classList.add("nav-link");

  // Replace the bootstrap-icons glyph with inline SVG sun/moon icons so
  // rendering never depends on a webfont being available/loaded correctly.
  var bi = toggle.querySelector(".bi");
  if (bi) {
    bi.style.display = "none";
  }

  var moonSVG =
    '<svg class="theme-icon theme-icon-moon" xmlns="http://www.w3.org/2000/svg" ' +
    'width="1em" height="1em" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">' +
    '<path d="M21.64 13a1 1 0 0 0-1.05-.14 8.05 8.05 0 0 1-3.37.73 8.15 8.15 0 0 1-8.14-8.1 8.59 8.59 0 0 1 .27-2.06A1 1 0 0 0 8.1 2.36a10.14 10.14 0 1 0 13.53 12.14 1 1 0 0 0-.02-1.5z"/>' +
    '</svg>';

  var sunSVG =
    '<svg class="theme-icon theme-icon-sun" xmlns="http://www.w3.org/2000/svg" ' +
    'width="1em" height="1em" viewBox="0 0 24 24" fill="none" stroke="currentColor" ' +
    'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
    '<circle cx="12" cy="12" r="4"/>' +
    '<line x1="12" y1="2" x2="12" y2="4"/>' +
    '<line x1="12" y1="20" x2="12" y2="22"/>' +
    '<line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/>' +
    '<line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/>' +
    '<line x1="2" y1="12" x2="4" y2="12"/>' +
    '<line x1="20" y1="12" x2="22" y2="12"/>' +
    '<line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/>' +
    '<line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/>' +
    "</svg>";

  toggle.insertAdjacentHTML("beforeend", moonSVG + sunSVG);

  li.appendChild(toggle);
  iconsList.insertBefore(li, iconsList.firstChild);

  if (toolsContainer && toolsContainer.children.length === 0) {
    toolsContainer.remove();
  }
});
