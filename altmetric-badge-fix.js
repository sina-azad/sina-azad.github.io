// Altmetric's embed script populates .altmetric-embed asynchronously, and
// leaves the container empty (but present) when a publication has no
// Altmetric data. It also tends to swap out intermediate content (e.g. a
// loading placeholder) before settling on the final badge.
//
// This just marks containers that end up with a real badge via the
// .has-badge class (after mutations settle, to avoid flagging an
// intermediate placeholder). Dark-mode CSS uses that class to paint a
// same-box ::before backdrop behind the badge for legibility — no
// coordinate math needed since it's pinned with inset:0 on the badge's own
// box, so it can never drift out of alignment.
document.addEventListener("DOMContentLoaded", function () {
  var badges = document.querySelectorAll(".altmetric-embed");

  badges.forEach(function (badge) {
    var settleTimer = null;

    var handleMutation = function () {
      if (settleTimer) {
        clearTimeout(settleTimer);
        settleTimer = null;
      }

      if (badge.childElementCount === 0) {
        badge.classList.remove("has-badge");
        return;
      }

      settleTimer = setTimeout(function () {
        badge.classList.add("has-badge");
      }, 300);
    };

    handleMutation();

    var observer = new MutationObserver(handleMutation);
    observer.observe(badge, { childList: true, subtree: true });
  });
});
