// WLR rail: each stage (Bring in, Organize, Correct, Report, More) folds under its heading and only the stage of the
// page shown is open, so the rail fits the screen. A heading opens its stage and folds the others; pages before the
// first heading (Site) always show.
(function () {
  function build() {
    var rail = document.querySelector('.d-rail');
    if (!rail || rail.querySelector('.wl-stage-body')) return;
    var kids = Array.prototype.slice.call(rail.children), body = null;
    kids.forEach(function (el) {
      if (el.classList.contains('d-rail-sec')) {
        body = document.createElement('div'); body.className = 'wl-stage-body';
        el.classList.add('wl-stage-head'); el.setAttribute('role', 'button'); el.setAttribute('tabindex', '0');
        var chev = document.createElement('span'); chev.className = 'wl-stage-chev'; chev.setAttribute('aria-hidden', 'true'); el.appendChild(chev);
        el.after(body); el._body = body; body._head = el;
        el.addEventListener('click', function () { open(el._body, true); });
        el.addEventListener('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); open(el._body, true); } });
      } else if (body) body.appendChild(el);
    });
    var act = rail.querySelector('.d-rail-link.active');
    open(act ? act.closest('.wl-stage-body') : null, false);
    new MutationObserver(function (ms) {                       // a page opened from anywhere (links, keys) opens its stage
      ms.forEach(function (m) { var a = m.target;
        if (a.classList && a.classList.contains('active') && a.classList.contains('d-rail-link')) { var b = a.closest('.wl-stage-body'); if (b) open(b, false); } });
    }).observe(rail, { subtree: true, attributes: true, attributeFilter: ['class'] });
  }
  function open(body, toggle) {
    var rail = document.querySelector('.d-rail');
    var shut = toggle && body && !body.classList.contains('wl-shut');   // a click on the open stage's heading folds it
    rail.querySelectorAll('.wl-stage-body').forEach(function (b) {
      var on = b === body && !shut; b.classList.toggle('wl-shut', !on); b._head.setAttribute('aria-expanded', on ? 'true' : 'false');
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', build); else build();
})();
