
// ovid Standard time-series plot (core/tsview.R). Times are seconds of logger time, labelled as UTC.
(function(){
  'use strict';
  var DAY = 86400, MON = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];
  var WINS = [['1 week', 7], ['2 weeks', 14], ['1 month', 30.5], ['3 months', 91], ['6 months', 182.5], ['1 year', 365.25]];
  var MIN = {half: 0.8, hourly: 0.8, daily: 0.8, monthly: 0.8, annual: 0.8};
  var O = {agg: 'half', spline: true, ghost: true, fill: '#5fb3b3', bars: true};
  try { var sv = JSON.parse(localStorage.getItem('ovid-tsv') || '{}'); for (var k in sv) if (k in O) O[k] = sv[k]; } catch (e) {}
  function save(){ try { localStorage.setItem('ovid-tsv', JSON.stringify(O)); } catch (e) {} }
  var V = {}, SPACE = false, HOV = null, ACT = null;   // the plots, by id; whether Space is held down; the page's cursor {id, t}; the plot last used (for keys)
  // Save image sizes (plot and table standards A10): layout width and height in CSS pixels, and the scale to the saved pixels (300 dpi for print)
  var PRESETS = {'85': {w: 340, h: 250, s: 1004 / 340, name: 'Journal, single column', sub: '85 mm \u00b7 300 dpi', tag: '85mm'},
                 '180': {w: 720, h: 380, s: 2126 / 720, name: 'Journal, double column', sub: '180 mm \u00b7 300 dpi', tag: '180mm'},
                 'slide': {w: 960, h: 540, s: 2, name: 'Slide', sub: '16:9 \u00b7 1920 \u00d7 1080 px', tag: 'slide'}};
  // the light print scheme, whatever the screen uses
  var LIGHT = {fg: '#20262b', fg4: '#5b646b', grid: '#e1e4e0', line: '#2f6f8f', warn: '#a77c00', err: '#b23a3a', gh: '#8a9399', ref: '#7a5195', bg: '#ffffff', bar: '#4a7fb0'};
  function pad(n){ return (n < 10 ? '0' : '') + n; }
  function css(n, d){ var v = getComputedStyle(document.documentElement).getPropertyValue(n); return v && v.trim() ? v.trim() : d; }
  function arr(x){ return x == null ? [] : Array.isArray(x) ? x : [x]; }
  // A stretch with no logger in place (span kind "absent", M 14 Oct): grey diagonal hatching, apart from the solid shading
  // of removed and out-of-water stretches; labelled on the first panel when there is room. Same drawing as workbench_chart.js.
  function hatch(c, x0, x1, y, h, col, label, font){ var a = x0, b = x1; if (b - a < 1) return; c.save(); c.beginPath(); c.rect(a, y, b - a, h); c.clip();
    c.strokeStyle = col; c.globalAlpha = 0.45; c.lineWidth = 1.2; c.beginPath(); for (var k = Math.floor((a - h) / 7) * 7; k < b; k += 7) { c.moveTo(k, y + h); c.lineTo(k + h, y); } c.stroke();
    if (label && b - a > 90) { c.globalAlpha = 0.95; c.fillStyle = col; c.font = font; c.textAlign = 'center'; c.textBaseline = 'top'; var w = c.measureText(label).width + 10, m = (a + b) / 2;
      c.globalCompositeOperation = 'destination-out'; c.fillRect(m - w / 2, y + 4, w, 15); c.globalCompositeOperation = 'source-over'; c.fillText(label, m, y + 6); }
    c.restore(); }
  function lower(a, x){ var lo = 0, hi = a.length; while (lo < hi) { var m = (lo + hi) >> 1; if (a[m] < x) lo = m + 1; else hi = m; } return lo; }
  function esc(s){ return String(s).replace(/[&<>"]/g, function(c){ return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;'}[c]; }); }
  function fd(t){ var d = new Date(t * 1000); return d.getUTCDate() + ' ' + MON[d.getUTCMonth()] + ' ' + d.getUTCFullYear(); }
  function ft(t){ var d = new Date(t * 1000); return d.getUTCFullYear() + '-' + pad(d.getUTCMonth() + 1) + '-' + pad(d.getUTCDate()) + ' ' + pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()); }
  function fiso(t){ return ft(t).replace(' ', 'T'); }
  function fv(v, dg){ return v == null || !isFinite(v) ? '\u2014' : (v < 0 ? '\u2212' : '') + Math.abs(v).toFixed(dg == null ? 2 : dg); }
  function nice(lo, hi, n){ var sp = hi - lo; if (!(sp > 0)) { var a = Math.abs(lo) || 1; lo -= a / 2; hi += a / 2; sp = hi - lo; }
    var raw = sp / n, mag = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / mag, s = (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * mag, out = [];
    for (var v = Math.ceil(lo / s) * s; v <= hi + s * 1e-9; v += s) out.push(Math.abs(v) < s * 1e-9 ? 0 : v);
    return {ticks: out, digits: Math.max(0, Math.min(6, -Math.floor(Math.log10(s) + 1e-9)))}; }
  var STEPS = [3600, 10800, 21600, 43200, DAY, 2 * DAY, 7 * DAY, 14 * DAY], MONTH = 2629746;
  function tticks(v0, v1, w){ var span = v1 - v0, want = Math.max(2, Math.floor(w / 95)), out = [], i, t;
    for (i = 0; i < STEPS.length; i++) if (span / STEPS[i] <= want) { var s = STEPS[i]; for (t = Math.ceil(v0 / s) * s; t <= v1; t += s) out.push(t); return {ticks: out, step: s}; }
    var MS = [1, 2, 3, 6, 12, 24, 60];
    for (i = 0; i < MS.length; i++) if (span / (MS[i] * MONTH) <= want || i === MS.length - 1) { var m = MS[i], d = new Date(v0 * 1000), y = d.getUTCFullYear(), mo = Math.floor(d.getUTCMonth() / m) * m;
      for (var g = 0; g < 400; g++) { t = Date.UTC(y, mo, 1) / 1000; if (t > v1) break; if (t >= v0) out.push(t); mo += m; } return {ticks: out, step: m * MONTH}; }
    return {ticks: out, step: span}; }
  function tlab(t, step, first){ var d = new Date(t * 1000), day = d.getUTCDate() + ' ' + MON[d.getUTCMonth()];
    if (step >= MONTH * 11.9) return '' + d.getUTCFullYear();
    if (step >= MONTH * 0.99) return MON[d.getUTCMonth()] + (d.getUTCMonth() === 0 || first ? ' ' + d.getUTCFullYear() : '');
    if (step >= DAY) return day + (first ? ' ' + d.getUTCFullYear() : '');
    var hm = pad(d.getUTCHours()) + ':' + pad(d.getUTCMinutes()); return first || (d.getUTCHours() === 0) ? day + ' ' + hm : hm; }

  // the key of the period a time falls in, and the period's start and length
  function bkey(t, a){ if (a === 'half') return Math.floor(t / 1800); if (a === 'hourly') return Math.floor(t / 3600); if (a === 'daily') return Math.floor(t / DAY);
    var d = new Date(t * 1000); return a === 'monthly' ? d.getUTCFullYear() * 12 + d.getUTCMonth() : d.getUTCFullYear(); }
  function bspan(k, a){ if (a === 'half') return [k * 1800, 1800]; if (a === 'hourly') return [k * 3600, 3600]; if (a === 'daily') return [k * DAY, DAY];
    if (a === 'monthly') { var s = Date.UTC(Math.floor(k / 12), k % 12, 1) / 1000; return [s, Date.UTC(Math.floor(k / 12), k % 12 + 1, 1) / 1000 - s]; }
    var s2 = Date.UTC(k, 0, 1) / 1000; return [s2, Date.UTC(k + 1, 0, 1) / 1000 - s2]; }
  // what the plot draws for a series at the current view setting: the records, or means (totals) per period
  function agg(P, s, key){ var a = O.agg; if (a === 'raw' || (a === 'half' && P.step >= 1800)) return {t: s.t, v: s[key] || s.v, bins: false};
    s._agg = s._agg || {}; var ck = a + ':' + key; if (s._agg[ck]) return s._agg[ck];
    var src = s[key] || s.v, T = [], Vv = [], ok = [], cur = null, sum = 0, n = 0, i;
    function push(){ if (cur === null) return; var sp = bspan(cur, a), want = sp[1] / Math.max(P.step, 1);
      T.push(sp[0] + sp[1] / 2); Vv.push(n ? (s.sum ? sum : sum / n) : null); ok.push(n >= MIN[a] * want); }
    for (i = 0; i < s.t.length; i++) { var k = bkey(s.t[i], a); if (k !== cur) { push(); cur = k; sum = 0; n = 0; } var y = src[i]; if (y != null && isFinite(y)) { sum += y; n++; } }
    push(); return (s._agg[ck] = {t: T, v: Vv, ok: ok, bins: true, label: a === 'half' ? 'half-hourly' : a}); }

  function Plot(el){ var self = this; this.el = el; this.id = el.id; this.H = +el.getAttribute('data-h') || 200; this.span = 91; this.c = null; this.close = null; this.d = null;
    el.innerHTML = '<div class="tsv-ctl"></div><div class="tsv-plot"><div class="tsv-act" hidden></div><canvas></canvas><div class="tsv-tip"></div></div><canvas class="tsv-nav"></canvas><div class="tsv-note"></div><div class="tsv-sel" hidden></div><div class="tsv-recs"></div>';
    this.ctl = el.querySelector('.tsv-ctl'); this.box = el.querySelector('.tsv-plot'); this.cv = this.box.querySelector('canvas'); this.tip = this.box.querySelector('.tsv-tip');
    this.nav = el.querySelector('.tsv-nav'); this.note = el.querySelector('.tsv-note'); this.recs = el.querySelector('.tsv-recs'); this.selBox = el.querySelector('.tsv-sel'); this.sel = null; this.act = this.box.querySelector('.tsv-act'); this.hist = [];
    this.cv.addEventListener('contextmenu', function(e){ e.preventDefault(); });
    if (window.ResizeObserver) new ResizeObserver(function(){ self.draw(); }).observe(el);
    this.cv.addEventListener('mousedown', function(e){ self.down(e, false); }); this.nav.addEventListener('mousedown', function(e){ self.down(e, true); });
    this.cv.addEventListener('wheel', function(e){ self.wheel(e); }, {passive: false});
    this.cv.addEventListener('mousemove', function(e){ if (!self.dr) { var q = self.pos(e); self.hx = q.x; ACT = self; self.cv.style.cursor = SPACE || self.onAxis(q) ? 'grab' : self.onVal(q) != null ? 'ns-resize' : 'crosshair'; self.draw(); cursor(self, self.tAt(e.clientX)); } });
    this.cv.addEventListener('mouseleave', function(){ self.hx = null; self.draw(); cursor(self, null); });
    this.cv.addEventListener('dblclick', function(){ self.mark(); self.span = self.full() / DAY; self.c = null; self.draw(); }); }
  var P = Plot.prototype;
  P.set = function(m){ m.series = arr(m.series); m.issues = arr(m.issues); m.spans = arr(m.spans); m.actions = arr(m.actions);
    m.series.forEach(function(s){ s.t = arr(s.t); s.v = arr(s.v); if (s.raw) s.raw = arr(s.raw); if (s.ref) s.ref = arr(s.ref); if (s.fill) s.fill = arr(s.fill);
      if (s.marks) { s.marks.t = arr(s.marks.t); s.marks.v = arr(s.marks.v); if (s.marks.text) s.marks.text = arr(s.marks.text); }
      s.dots = arr(s.dots).map(function(x){ x.t = arr(x.t); x.v = arr(x.v); return x; }); });
    // the panels: one per series, or one for every series that names the same panel (drawn in its own colour)
    var pn = [], by = {}; m.series.forEach(function(s, i){ var k = s.panel ? 'p:' + s.panel : 's:' + i;
      if (!(k in by)) { by[k] = pn.length; pn.push({key: s.panel || s.key || s.name, lab: s.plab || s.name + (s.unit ? ' (' + s.unit + ')' : ''), ss: []}); } pn[by[k]].ss.push(s); });
    m.pn = pn; if (this.band && this.band.p >= pn.length) this.band = null;
    if (m.bars) { m.bars.t = arr(m.bars.t); m.bars.v = arr(m.bars.v); }
    var same = this.d && this.d.series.length && m.series.length && this.d.series[0].name === m.series[0].name && this.d.series[0].t[0] === m.series[0].t[0];
    this.d = m; this.step = m.step || 1800;
    if (!same) { this.close = null; this.span = Math.min(91, this.full() / DAY); var f = m.focus != null ? m.focus : m.issues.length ? (m.issues[0].t0 + m.issues[0].t1) / 2 : null;
      this.c = f; }
    else if (m.focus != null) { this.close = null; this.c = m.focus; }      // the same data opened at a given time
    this.draw(); };
  P.rng = function(){ var lo = Infinity, hi = -Infinity; (this.d ? this.d.series : []).forEach(function(s){ if (s.t.length) { lo = Math.min(lo, s.t[0]); hi = Math.max(hi, s.t[s.t.length - 1]); } }); return isFinite(lo) ? [lo, hi] : [0, 1]; };
  P.full = function(){ var r = this.rng(); return Math.max(r[1] - r[0], 3600); };
  // the window: span days around c (the latest records when c is not set), kept inside the record
  P.win = function(){ var r = this.rng(), w = Math.min(this.span * DAY, r[1] - r[0] || DAY), c = this.c == null ? r[1] - w / 2 : this.c;
    var a = Math.max(r[0], Math.min(c - w / 2, r[1] - w)); this.c = a + w / 2; return [a, a + w]; };
  P.zoom = function(f, at){ var w = this.win(), ns = Math.max(0.25, Math.min(this.full() / DAY, this.span * f)); if (at != null) this.c = at + (this.c - at) * ns / this.span; this.span = ns; this.draw(); };
  P.pos = function(e){ var r = this.cv.getBoundingClientRect(); return {x: e.clientX - r.left, y: e.clientY - r.top, w: r.width}; };
  // the view so far, for Back (Backspace); kept short
  P.mark = function(){ this.hist.push({span: this.span, c: this.c}); if (this.hist.length > 30) this.hist.shift(); };
  P.goBack = function(){ var h = this.hist.pop(); if (!h) return false; this.close = null; this.span = h.span; this.c = h.c; this.draw(); return true; };
  P.onAxis = function(q){ return q.y > this.cv.getBoundingClientRect().height - 24; };    // the time axis band under the panels
  // the panel whose value axis (left of the panels) is under the pointer, or null
  P.onVal = function(q){ if (!this.geo || q.x >= this.L - 2) return null; for (var i = 0; i < this.geo.length; i++) { var g = this.geo[i]; if (q.y >= g.y0 && q.y <= g.y0 + g.ph) return i; } return null; };
  P.vAt = function(i, y){ var g = this.geo[i]; return g.lo + (g.y0 + g.ph - y) / g.ph * (g.hi - g.lo); };
  P.down = function(e, nav){ if (!this.d) return; ACT = this;
    if (!nav && e.button === 0 && !SPACE) { var vp = this.onVal(this.pos(e)); if (vp != null) return this.bandDrag(e, vp); }
    if (!nav && e.button === 0 && !SPACE && !e.shiftKey && !this.onAxis(this.pos(e))) return this.zoomDrag(e);       // drag on the plot: right zooms in, left zooms out
    if (!nav && (e.button === 2 || (e.button === 0 && !SPACE && !this.onAxis(this.pos(e))))) return this.select(e);   // Shift + drag (or right-drag): select
    if (e.button !== 0) return; e.preventDefault(); this.mark(); var self = this, q = this.pos(e), w0 = this.win(), c0 = this.c, x0 = e.clientX, r = this.rng(),
      rect = (nav ? this.nav : this.cv).getBoundingClientRect(), L = nav ? 0 : this.L;
    function navAt(cx){ return r[0] + (cx - rect.left) / rect.width * (r[1] - r[0]); }
    if (nav) { this.c = navAt(e.clientX); this.draw(); }
    this.dr = true; this.box.classList.add('drag');
    function mv(ev){ if (nav) self.c = navAt(ev.clientX); else self.c = c0 - (ev.clientX - x0) / (rect.width - L - self.R) * (w0[1] - w0[0]); self.draw(); }
    function up(){ window.removeEventListener('mousemove', mv); window.removeEventListener('mouseup', up); self.dr = false; self.box.classList.remove('drag'); }
    window.addEventListener('mousemove', mv); window.addEventListener('mouseup', up); };
  // a selected stretch: shaded on the plot, with the figures of each series inside it under the plot
  P.tAt = function(cx){ var r = this.cv.getBoundingClientRect(), w = this.win(), pw = r.width - this.L - this.R; return w[0] + Math.max(0, Math.min(pw, cx - r.left - this.L)) / pw * (w[1] - w[0]); };
  P.select = function(e){ e.preventDefault(); var self = this, a = this.tAt(e.clientX), x0 = e.clientX, base = e.shiftKey ? this.sel : null; this.dr = true; if (!base) this.band = null;
    function join(t0, t1){ return base ? {t0: Math.min(base.t0, t0), t1: Math.max(base.t1, t1)} : {t0: t0, t1: t1}; }
    function mv(ev){ if (Math.abs(ev.clientX - x0) < 4) return; var b = self.tAt(ev.clientX); self.sel = join(Math.min(a, b), Math.max(a, b)); self.draw(); }
    function up(ev){ window.removeEventListener('mousemove', mv); window.removeEventListener('mouseup', up); self.dr = false;
      if (Math.abs(ev.clientX - x0) < 4) { var t = self.near(ev); self.sel = t == null ? base : join(t, t); } self.draw(); }
    window.addEventListener('mousemove', mv); window.addEventListener('mouseup', up); };
  // a plain drag (M 14 Oct): left to right zooms to the dragged stretch; right to left zooms out (the view doubles around the
  // stretch, up to the whole record); under 6 px it is a click, which selects the nearest record
  P.zoomDrag = function(e){ e.preventDefault(); var self = this, a = this.tAt(e.clientX), x0 = e.clientX; this.dr = true;
    function mv(ev){ if (Math.abs(ev.clientX - x0) < 6) { self.zb = null; return self.draw(); } var b = self.tAt(ev.clientX); self.zb = {t0: Math.min(a, b), t1: Math.max(a, b), out: ev.clientX < x0}; self.draw(); }
    function up(ev){ window.removeEventListener('mousemove', mv); window.removeEventListener('mouseup', up); self.dr = false; var z = self.zb; self.zb = null;
      if (Math.abs(ev.clientX - x0) < 6 || !z) { self.band = null; var t = self.near(ev); self.sel = t == null ? null : {t0: t, t1: t}; return self.draw(); }
      self.mark(); var full = self.full() / DAY;
      if (!z.out) { self.span = Math.max(0.25, Math.min(full, (z.t1 - z.t0) / DAY)); self.c = (z.t0 + z.t1) / 2; }
      else { self.span = Math.min(full, self.span * 2); self.c = (z.t0 + z.t1) / 2; }
      self.draw(); }
    window.addEventListener('mousemove', mv); window.addEventListener('mouseup', up); };
  // the time of the record nearest a click, within 8 px, or null
  P.near = function(ev){ var s = this.d.series[0], A = agg(this, s, 'v'), t = this.tAt(ev.clientX), i = lower(A.t, t), w = this.win(), pw = this.cv.getBoundingClientRect().width - this.L - this.R, best = null, bd = 8 / pw * (w[1] - w[0]);
    for (var k = Math.max(0, i - 1); k <= Math.min(A.t.length - 1, i); k++) if (A.v[k] != null && Math.abs(A.t[k] - t) <= bd) { bd = Math.abs(A.t[k] - t); best = A.t[k]; }
    return best; };
  // a value band: drag up or down the value axis of a panel to narrow the selection to the values between (the stretch is the
  // selection, or the whole view when nothing is selected); a click on the axis drops the band
  P.bandDrag = function(e, pi){ e.preventDefault(); var self = this, y0 = this.pos(e).y, g = this.geo[pi], a = this.vAt(pi, y0), made = !this.sel; this.dr = true;
    if (made) { var w = this.win(); this.sel = {t0: w[0], t1: w[1]}; }
    function mv(ev){ var y = Math.max(g.y0, Math.min(g.y0 + g.ph, self.pos(ev).y)); if (Math.abs(y - y0) < 3) return; var b = self.vAt(pi, y); self.band = {p: pi, lo: Math.min(a, b), hi: Math.max(a, b)}; self.draw(); }
    function up(ev){ window.removeEventListener('mousemove', mv); window.removeEventListener('mouseup', up); self.dr = false;
      if (Math.abs(self.pos(ev).y - y0) < 3) { self.band = null; if (made) self.sel = null; } self.draw(); }
    window.addEventListener('mousemove', mv); window.addEventListener('mouseup', up); };
  // the records inside the band and the selected stretch, over the band's panel
  P.inBand = function(){ var bd = this.band, q = this.sel, n = 0; if (!bd || !q) return 0;
    this.d.pn[bd.p].ss.forEach(function(s){ for (var i = lower(s.t, q.t0 - 1e-6); i < s.t.length && s.t[i] <= q.t1 + 1e-6; i++) { var y = s.v[i]; if (y != null && y >= bd.lo && y <= bd.hi) n++; } }); return n; };
  P.zoomSel = function(){ var q = this.sel; if (!q) return; this.mark(); var pad = Math.max((q.t1 - q.t0) * 0.15, this.step * 4); this.span = Math.max(0.25, (q.t1 - q.t0 + 2 * pad) / DAY); this.c = (q.t0 + q.t1) / 2; this.draw(); };
  P.count = function(t0, t1){ var s = this.d.series[0]; return lower(s.t, t1 + 1e-6) - lower(s.t, t0); };
  // the bar above the selection: how much is selected and what can be done with it
  // with page actions (ts_view_send actions): the edits the page offers for the stretch (or the band), and the reason, initials
  // and any other field it asks for; the form is kept while the selection changes, so what was typed stays
  P.drawAct = function(){ var b = this.act, q = this.sel, d = this.d; if (!q || this.dr) { b.hidden = true; return; }
    var w = this.win(), W = this.box.clientWidth, pw = W - this.L - this.R, n = this.count(q.t0, q.t1), href = this.wbHref(q.t0, q.t1), bd = this.band, bp = bd ? d.pn[bd.p] : null, acts = d.actions || [];
    if (!b.querySelector('.h')) b.innerHTML = '<div class="h"></div><div class="f" hidden></div>';
    var dg = bp && bp.ss[0].digits != null ? bp.ss[0].digits : 2, short = bp ? bp.lab.replace(/ \([^)]*\)$/, '') : '';
    var h = '<span class="n">' + n + (n === 1 ? ' record' : ' records') + ' · ' + ft(q.t0) + (q.t1 > q.t0 ? ' to ' + ft(q.t1) : '') + (d.zone ? ' (' + esc(d.zone) + ')' : '') + '</span>' +
      (bp ? '<span class="n">· ' + esc(short) + ' between ' + fv(bd.lo, dg) + ' and ' + fv(bd.hi, dg) + ' (' + this.inBand() + ' points)</span>' : '') +
      '<button type="button" class="btn btn-default btn-xs" data-tsv-go="selzoom">Zoom to this</button><button type="button" class="btn btn-default btn-xs" data-tsv-go="selcloser">Look closer</button>' +
      (href ? '<a class="btn btn-default btn-xs" href="' + esc(href) + '" target="_blank">Open in Workbench</a>' : '') + '<button type="button" class="btn btn-default btn-xs" data-tsv-go="selclear" title="Esc">Clear</button>' +
      acts.filter(function(a){ return a.band == null || !!a.band === !!bd; }).map(function(a){ return '<button type="button" class="btn btn-default btn-xs tsv-a' + (a.kind ? ' ' + esc(a.kind) : '') + '" data-tsv-act="' + esc(a.id) + '"' + (a.title ? ' title="' + esc(a.title) + '"' : '') + '>' +
        esc(bd && a.label_band ? a.label_band.replace('{panel}', short) : a.label) + '</button>'; }).join('');
    if (h !== this.actH) { this.actH = h; b.querySelector('.h').innerHTML = h; }
    var f = b.querySelector('.f'); if (acts.length && !f.firstChild) f.innerHTML = this.form(); f.hidden = !acts.length; b.classList.toggle('wide', acts.length > 0);
    b.hidden = false; if (acts.length) { b.style.left = '0px'; return; } var x = this.L + ((q.t0 + q.t1) / 2 - w[0]) / (w[1] - w[0]) * pw, bw = b.offsetWidth; b.style.left = Math.max(0, Math.min(W - bw, x - bw / 2)) + 'px'; };
  P.form = function(){ var a = this.d.ask || {}, me = ''; try { me = localStorage.getItem('ovid-initials') || ''; } catch (e) {}
    return (a.choices ? '<label>' + esc(a.choices_label || 'Why') + ' <select data-tsv-f="cause">' + arr(a.choices).map(function(c){ return '<option>' + esc(c) + '</option>'; }).join('') + '</select></label>' : '') +
      '<input type="text" class="r" data-tsv-f="reason" placeholder="Reason' + (a.reason === false ? '' : ' (needed)') + '" aria-label="Reason">' +
      '<input type="text" class="i" data-tsv-f="author" placeholder="Initials" aria-label="Initials" value="' + esc(me) + '">' + (a.note ? '<span class="dim">' + esc(a.note) + '</span>' : ''); };
  // a page action: input <id>_act = {act, t0, t1, key, lo, hi, n, reason, author, cause}; key, lo and hi are the band's (null without one)
  P.doAct = function(id){ var d = this.d, q = this.sel, bd = this.band, f = this.act.querySelector('.f'), a = d.ask || {}; if (!q) return;
    function g(k){ var x = f && f.querySelector('[data-tsv-f="' + k + '"]'); return x ? x.value.trim() : ''; }
    if (a.reason !== false && !g('reason')) { var r = f.querySelector('[data-tsv-f="reason"]'); r.classList.add('need'); r.focus(); return; }
    try { localStorage.setItem('ovid-initials', g('author')); } catch (e) {}
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(this.id + '_act', {act: id, t0: q.t0, t1: q.t1, key: bd ? d.pn[bd.p].key : null, lo: bd ? bd.lo : null, hi: bd ? bd.hi : null,
      n: bd ? this.inBand() : this.count(q.t0, q.t1), reason: g('reason'), author: g('author'), cause: g('cause'), at: Date.now()}, {priority: 'event'}); };
  // after the page wrote an action (ts_view_done): what was done, with Undo while the table store can take it back
  P.done = function(m){ this.sel = null; this.band = null; var r = this.act.querySelector('[data-tsv-f="reason"]'); if (r) { r.value = ''; r.classList.remove('need'); }
    var b = this.box.querySelector('.tsv-done'); if (!b) { b = document.createElement('div'); b.className = 'tsv-done'; b.setAttribute('role', 'status'); this.box.appendChild(b); }
    b.innerHTML = '<span>' + esc(m.msg) + '</span>' + (m.undo ? '<button type="button" class="btn btn-default btn-xs" data-tsv-undo="1" title="Ctrl/Cmd+Z">Undo</button>' : '') + '<button type="button" class="x" data-tsv-undo="0" aria-label="Close">×</button>';
    b.hidden = false; clearTimeout(this.doneT); this.doneT = setTimeout(function(){ b.hidden = true; }, 15000); this.draw(); };
  P.clearSel = function(){ this.sel = null; this.band = null; this.draw(); };
  P.drawSel = function(){ var b = this.selBox, q = this.sel, d = this.d; 
    // the selected stretch goes to the app as input <id>_sel = {t0, t1} (null when cleared), once the drag ends
    var bd = this.band, sk = q ? q.t0 + ':' + q.t1 + (bd ? ':' + bd.p + ':' + bd.lo + ':' + bd.hi : '') : '';
    if (!this.dr && sk !== this.selKey && window.Shiny && Shiny.setInputValue) { this.selKey = sk; Shiny.setInputValue(this.id + '_sel', q ? {t0: q.t0, t1: q.t1, key: bd ? d.pn[bd.p].key : null, lo: bd ? bd.lo : null, hi: bd ? bd.hi : null} : null); }
    if (!q || !d) { b.hidden = true; b.innerHTML = ''; return; }
    var rows = d.series.map(function(s){ var i0 = lower(s.t, q.t0), i1 = lower(s.t, q.t1 + 1e-6), n = 0, sm = 0, mn = Infinity, mx = -Infinity, first = null, last = null, miss = 0, i;
      for (i = i0; i < i1; i++) { var y = s.v[i]; if (y == null || !isFinite(y)) { miss++; continue; } n++; sm += y; if (y < mn) mn = y; if (y > mx) mx = y; if (first === null) first = y; last = y; }
      var dg = s.digits == null ? 2 : s.digits, f = function(v){ return fv(v, dg); };
      return '<tr><td>' + esc(s.name) + (s.unit ? ' (' + esc(s.unit) + ')' : '') + '</td><td>' + (n + miss) + '</td><td>' + miss + '</td><td>' + (n ? f(s.sum ? sm : sm / n) : '\u2014') + '</td><td>' + (n ? f(mn) : '\u2014') + '</td><td>' + (n ? f(mx) : '\u2014') +
        '</td><td>' + (n > 1 ? (last - first >= 0 ? '+' : '') + f(last - first) : '\u2014') + '</td></tr>'; });
    var hrs = (q.t1 - q.t0) / 3600;
    b.innerHTML = '<div class="row1"><b>Selected: ' + ft(q.t0) + ' to ' + ft(q.t1) + ' (' + (hrs < 48 ? Math.round(hrs * 10) / 10 + ' hours' : Math.round(hrs / 2.4) / 10 + ' days') + ')</b><span>' +
      '</span></div>' +
      '<table><thead><tr><th>Series</th><th>Records</th><th>Missing</th><th>Mean' + (d.series.some(function(s){ return s.sum; }) ? ' (total)' : '') + '</th><th>Min</th><th>Max</th><th>Change</th></tr></thead><tbody>' + rows.join('') + '</tbody></table>';
    b.hidden = false; };
  P.wheel = function(e){ if (!this.d) return; e.preventDefault(); var q = this.pos(e), w = this.win(), u = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1,
      dx = (e.shiftKey ? e.deltaY : e.deltaX) * u, dy = e.shiftKey ? 0 : e.deltaY * u, pw = q.w - this.L - this.R;
    if (Math.abs(dx) > Math.abs(dy)) { this.c += dx / pw * (w[1] - w[0]); this.draw(); return; }
    this.zoom(Math.min(1.2, Math.max(1 / 1.2, Math.exp(dy * 0.0015))), w[0] + (q.x - this.L) / pw * (w[1] - w[0])); };
  P.closer = function(t0, t1){ this.mark(); this.close = {t0: t0, t1: t1 == null ? t0 : t1}; this.span = Math.max(2, (this.close.t1 - this.close.t0) / DAY + 1); this.c = (this.close.t0 + this.close.t1) / 2; this.draw();
    // bring the plot's top into view, below whatever bars are fixed at the top of the page
    var hide = 0; document.querySelectorAll('.d-top,.d-tabs').forEach(function(b){ var cs = getComputedStyle(b); if (cs.position === 'fixed' || cs.position === 'sticky') hide = Math.max(hide, b.getBoundingClientRect().bottom); });
    var top = this.el.getBoundingClientRect().top - hide - 12; if (Math.abs(top) > 4) window.scrollBy({top: top, behavior: 'smooth'}); };
  P.back = function(){ if (this.goBack()) return; this.close = null; this.span = Math.min(91, this.full() / DAY); this.draw(); };

  P.draw = function(){ var d = this.d; if (!d) return; var self = this, PR = this.print, W = PR ? PR.w : this.box.clientWidth; if (W < 20) return;   // PR: drawing a saved image (P.saveImage)
    var dpr = PR ? PR.s : window.devicePixelRatio || 1, ns = d.pn.length, bars = d.bars && O.bars && d.bars.t.length, BH = bars ? 34 : 0, CAP = PR ? PR.cap : 0, H = PR ? PR.h - CAP : (d.height || this.H) * Math.max(1, ns * 0.75) + BH;
    this.L = 58; this.R = 10; var T = PR ? 40 : 8, B = PR ? 38 : 24, G = 10, w = this.win(), v0 = w[0], v1 = w[1], pw = W - this.L - this.R, cv = PR ? PR.cv : this.cv;
    cv.width = Math.round(W * dpr); cv.height = Math.round((H + CAP) * dpr); if (!PR) cv.style.height = H + 'px'; var c = cv.getContext('2d'); c.setTransform(dpr, 0, 0, dpr, 0, 0); c.clearRect(0, 0, W, H + CAP);
    var C = PR ? Object.assign({font: getComputedStyle(document.body).fontFamily}, LIGHT) : {fg: css('--fg', '#ddd'), fg4: css('--fg4', '#aaa'), grid: css('--bg2', '#333'), line: css('--acc', '#8bd'), warn: css('--yellow', '#eb6'), err: css('--red', '#c66'),
             gh: css('--gray', '#888'), ref: css('--purple', css('--fg4', '#c9c')), bg: css('--bg', '#222'), bar: css('--blue', '#69c'), font: getComputedStyle(document.body).fontFamily};
    if (PR) { c.fillStyle = C.bg; c.fillRect(0, 0, W, H + CAP); }
    var ph = (H - BH - T - B - G * (ns - 1)) / ns, X = function(t){ return self.L + (t - v0) / (v1 - v0) * pw; }, tk = tticks(v0, v1, pw), notes = [];
    // same scale: every panel takes the lowest and highest value shown in any of them
    var GL = Infinity, GH = -Infinity; if (d.same) d.series.forEach(function(s){ var A = agg(self, s, 'v'), e = Math.min(A.t.length, lower(A.t, v1) + 1);
      for (var i = Math.max(0, lower(A.t, v0) - 1); i < e; i++) { var y = A.v[i]; if (y != null && isFinite(y)) { if (y < GL) GL = y; if (y > GH) GH = y; } } });
    if (!PR) this.geo = [];
    d.pn.forEach(function(pn, si){ var y0 = T + si * (ph + G), lo = Infinity, hi = -Infinity, i, one = pn.ss.length === 1;
      // each series of the panel: what it draws at this view, and the part of it in the window
      var S = pn.ss.map(function(s){ var A = agg(self, s, 'v'); return {s: s, A: A, i0: Math.max(0, lower(A.t, v0) - 1), i1: Math.min(A.t.length, lower(A.t, v1) + 1),
        R: s.raw && O.ghost ? agg(self, s, 'raw') : null, M: s.marks && s.marks.t && s.marks.t.length ? s.marks : null, RF: s.ref ? agg(self, s, 'ref') : null, col: s.col || C.line}; });
      S.forEach(function(o){ var i0 = o.i0, i1 = o.i1;
        [o.A, self.close || self.span <= 7 ? o.R : null, o.M, o.RF].forEach(function(Q){ if (!Q) return; for (var k = Q === o.M ? 0 : i0; k < (Q === o.M ? Q.v.length : Math.min(i1, Q.v.length)); k++) { if (Q === o.M && (Q.t[k] < v0 || Q.t[k] > v1)) continue; var y = Q.v[k]; if (y != null && isFinite(y)) { if (y < lo) lo = y; if (y > hi) hi = y; } } }); });
      if (isFinite(GL)) { lo = GL; hi = GH; }
      if (!isFinite(lo)) { lo = 0; hi = 1; } var pd = (hi - lo) * 0.08 || Math.abs(hi) * 0.05 || 0.5; lo -= pd; hi += pd;
      if (!PR) self.geo.push({y0: y0, ph: ph, lo: lo, hi: hi});
      var Y = function(y){ return y0 + ph - (y - lo) / (hi - lo) * ph; }, yt = nice(lo, hi, Math.max(2, Math.floor(ph / 36)));
      c.save(); c.beginPath(); c.rect(self.L, y0, pw, ph); c.clip(); c.strokeStyle = C.grid; c.lineWidth = 1;
      tk.ticks.forEach(function(t){ var x = Math.round(X(t)) + 0.5; c.beginPath(); c.moveTo(x, y0); c.lineTo(x, y0 + ph); c.stroke(); });
      yt.ticks.forEach(function(y){ var yy = Math.round(Y(y)) + 0.5; c.beginPath(); c.moveTo(self.L, yy); c.lineTo(self.L + pw, yy); c.stroke(); });
      d.issues.forEach(function(q){ if (q.t1 < v0 || q.t0 > v1) return; c.globalAlpha = 0.18; c.fillStyle = q.level === 'error' ? C.err : C.warn;
        var a = X(q.t0 - self.step / 2), b = X(q.t1 + self.step / 2); c.fillRect(a, y0, Math.max(3, b - a), ph); c.globalAlpha = 1; });
      d.spans.forEach(function(q){ if (q.t1 < v0 || q.t0 > v1) return; var a = X(q.t0), b = X(q.t1);     // spans the page shades: out of the water, removed by hand; not deployed hatched
        if (q.kind === 'absent') return hatch(c, a, b, y0, ph, C.gh, si === 0 ? 'not deployed' : null, '11px ' + C.font);
        c.globalAlpha = 0.22; c.fillStyle = C.gh; c.fillRect(a, y0, Math.max(2, b - a), ph); c.globalAlpha = 1; });
      if (self.zb && !PR) { c.globalAlpha = 0.14; c.fillStyle = self.zb.out ? C.warn : C.line; var za = X(self.zb.t0), zz = X(self.zb.t1); c.fillRect(za, y0, Math.max(1, zz - za), ph); c.globalAlpha = 1;
        if (si === 0) { c.save(); c.fillStyle = C.line; c.font = '11px sans-serif'; c.fillText(self.zb.out ? 'zoom out' : 'zoom in', za + 4, y0 + 12); c.restore(); } }
      if (self.sel && !PR) { c.globalAlpha = 0.16; c.fillStyle = C.line; var sa = X(self.sel.t0), sb = X(self.sel.t1); c.fillRect(sa, y0, Math.max(1, sb - sa), ph); c.globalAlpha = 0.8; c.strokeStyle = C.line;
        c.beginPath(); c.moveTo(Math.round(sa) + 0.5, y0); c.lineTo(Math.round(sa) + 0.5, y0 + ph); c.moveTo(Math.round(sb) + 0.5, y0); c.lineTo(Math.round(sb) + 0.5, y0 + ph); c.stroke(); c.globalAlpha = 1;
        var bd = self.band; if (bd && bd.p === si) { var ya = Y(bd.hi), yb = Y(bd.lo), xa = Math.max(self.L, sa), xb = Math.min(self.L + pw, sb); c.globalAlpha = 0.3; c.fillStyle = C.warn; c.fillRect(xa, ya, Math.max(1, xb - xa), Math.max(1, yb - ya)); c.globalAlpha = 1;
          c.strokeStyle = C.warn; c.setLineDash([4, 3]); c.strokeRect(Math.round(xa) + 0.5, Math.round(ya) + 0.5, Math.max(1, xb - xa - 1), Math.max(1, yb - ya)); c.setLineDash([]); } }
      S.forEach(function(o, oi){ var s = o.s, A = o.A, R = o.R, M = o.M, RF = o.RF, i0 = o.i0, i1 = o.i1;
        if (R) { c.strokeStyle = C.gh; c.globalAlpha = 0.7; c.setLineDash([3, 3]); c.lineWidth = 1; c.beginPath(); self.line(c, R, i0, i1, X, Y, pw, false); c.stroke(); c.setLineDash([]); c.globalAlpha = 1; }
        var sm = false;
        if (s.sum) { var cw = Math.max(1, (X(A.t[Math.min(i1, A.t.length) - 1] || v1) - X(A.t[i0] || v0)) / Math.max(1, i1 - i0) * 0.8), zy = Y(Math.max(lo, 0)); c.fillStyle = o.col;
          for (i = i0; i < i1; i++) if (A.v[i] != null && A.v[i] !== 0) { var yy = Y(A.v[i]); c.globalAlpha = A.bins && !A.ok[i] ? 0.4 : 0.9; c.fillRect(X(A.t[i]) - cw / 2, Math.min(yy, zy), cw, Math.max(1, Math.abs(zy - yy))); } c.globalAlpha = 1; }
        else { c.strokeStyle = o.col; c.lineWidth = one ? 1.5 : 1.2; c.lineJoin = 'round'; c.beginPath(); sm = self.line(c, A, i0, i1, X, Y, pw, O.spline); c.stroke(); }
        if (RF) { c.strokeStyle = C.ref; c.lineWidth = 1; c.globalAlpha = 0.9; c.beginPath(); self.line(c, RF, Math.max(0, lower(RF.t, v0) - 1), Math.min(RF.t.length, lower(RF.t, v1) + 1), X, Y, pw, O.spline); c.stroke(); c.globalAlpha = 1;
          notes.push('thin line: ' + (s.refname || 'reference')); }
        if (s.fill && !A.bins) { c.strokeStyle = O.fill; c.lineWidth = 2.2; c.beginPath(); var pen = false;
          for (i = i0; i < i1; i++) { if (!s.fill[i] || s.v[i] == null) { pen = false; continue; } var px = X(s.t[i]), py = Y(s.v[i]); if (!pen) { c.moveTo(px, py); pen = true; } else c.lineTo(px, py); } c.stroke(); }
        if (A.bins && !s.sum && i1 - i0 <= 160) for (i = i0; i < i1; i++) { if (A.v[i] == null) continue; c.beginPath(); c.arc(X(A.t[i]), Y(A.v[i]), i1 - i0 > 60 ? 2 : 3, 0, 6.2832);
          c.fillStyle = A.ok[i] ? o.col : C.bg; c.fill(); c.strokeStyle = o.col; c.lineWidth = 1.2; c.stroke(); }
        // dots the page marks on the records: removed (a grey cross at the value it had) and flagged (a gold ring); not part of the scale
        s.dots.forEach(function(D){ var nd = 0, a = lower(D.t, v0), b = lower(D.t, v1 + 1e-6), every = Math.max(1, Math.floor((b - a) / 4000)), rm = D.kind !== 'flagged'; c.lineWidth = 1.2; c.strokeStyle = rm ? C.gh : C.warn;
          c.beginPath(); for (var k = a; k < b; k += every) { var y = D.v[k]; if (y == null) continue; var dx = X(D.t[k]), dy = Y(y); nd++;
            if (rm) { c.moveTo(dx - 2.5, dy - 2.5); c.lineTo(dx + 2.5, dy + 2.5); c.moveTo(dx - 2.5, dy + 2.5); c.lineTo(dx + 2.5, dy - 2.5); } else { c.moveTo(dx + 3, dy); c.arc(dx, dy, 3, 0, 6.2832); } } c.stroke();
          if (nd) notes.push((rm ? '× ' : '○ ') + (D.label || D.kind)); });
        if (M) { var nm = 0; c.font = '10px ' + C.font; c.textAlign = 'left'; c.textBaseline = 'bottom';
          for (i = 0; i < M.t.length; i++) { if (M.t[i] < v0 || M.t[i] > v1 || M.v[i] == null) continue; var mx = X(M.t[i]), my = Y(M.v[i]); nm++;
            c.beginPath(); c.moveTo(mx, my - 5); c.lineTo(mx + 5, my); c.lineTo(mx, my + 5); c.lineTo(mx - 5, my); c.closePath(); c.fillStyle = C.fg; c.fill(); c.strokeStyle = C.bg; c.lineWidth = 1; c.stroke();
            var mt = M.text && M.text[i] != null && M.text[i] !== '' ? M.text[i] : M.text ? '' : M.label; if (mt) { c.fillStyle = C.fg4; c.fillText(mt, mx + 7, my - 3); } }
          if (nm) notes.push('◆ ' + (M.label || 'marks') + ' (' + nm + ' in view)'); }
        if (self.close && !PR) for (i = lower(s.t, self.close.t0 - 1); i < s.t.length && s.t[i] <= self.close.t1 + 1; i++) if (s.v[i] != null) { c.beginPath(); c.arc(X(s.t[i]), Y(s.v[i]), 3, 0, 6.2832); c.fillStyle = C.warn; c.fill(); }
        if (oi === 0) { if (A.bins) { var inc = 0; for (i = i0; i < i1; i++) if (!A.ok[i] && A.v[i] != null) inc++; notes.push((i1 - i0) + ' ' + A.label + (s.sum ? ' totals' : ' means') + (inc ? ', ' + inc + (s.sum ? ' faint' : ' hollow') + ' (short of the completeness rule)' : '')); }
          else if (sm) notes.push('smoothed for display'); } });
      // a panel of several series: their names in their colours along its top
      if (!one) { c.font = '10.5px ' + C.font; c.textBaseline = 'top'; c.textAlign = 'left'; var lx = self.L + pw - 6, items = S.map(function(o){ return [o.s.name, o.col]; }).reverse();
        items.forEach(function(it){ var tw = c.measureText(it[0]).width; lx -= tw; if (lx < self.L + 120) return; c.globalAlpha = 0.85; c.fillStyle = C.bg; c.fillRect(lx - 14, y0 + 3, tw + 16, 14); c.globalAlpha = 1;
          c.fillStyle = it[1]; c.fillRect(lx - 11, y0 + 9, 8, 2.5); c.fillStyle = C.fg4; c.fillText(it[0], lx, y0 + 4); lx -= 22; }); }
      c.restore();
      c.strokeStyle = C.grid; c.strokeRect(self.L + 0.5, y0 + 0.5, pw - 1, ph - 1);
      c.fillStyle = C.fg4; c.font = '10.5px ' + C.font; c.textAlign = 'right'; c.textBaseline = 'middle';
      yt.ticks.forEach(function(y){ var yy = Y(y); if (yy > y0 + 6 && yy < y0 + ph - 4) c.fillText(y.toFixed(yt.digits).replace('-', '−'), self.L - 6, yy); });
      var lab = pn.lab;
      if (PR) { c.save(); c.textAlign = 'left'; c.textBaseline = 'top'; c.fillStyle = C.fg; c.font = '500 10.5px ' + C.font; c.globalAlpha = 0.85; c.fillStyle = C.bg; c.fillRect(self.L + 2, y0 + 2, c.measureText(lab).width + 6, 15); c.globalAlpha = 1; c.fillStyle = C.fg; c.fillText(lab, self.L + 5, y0 + 4); c.restore(); }   // saved image: the full name and unit, in the panel
      else { c.save(); c.translate(12, y0 + ph / 2); c.rotate(-Math.PI / 2); c.textAlign = 'center'; c.fillStyle = C.fg; c.font = '500 11px ' + C.font;
        var room = Math.floor(ph / 6.2); c.fillText(lab.length > room ? lab.slice(0, room - 1) + '…' : lab, 0, 0); c.restore(); } });
    if (bars) { var by = H - B - BH + 6, bh = BH - 8, b = d.bars, j0 = lower(b.t, v0), j1 = lower(b.t, v1), mx = 0, tot = 0, j;
      for (j = j0; j < j1; j++) if (b.v[j] > mx) mx = b.v[j]; c.fillStyle = C.bar; c.globalAlpha = 0.8; var bw = Math.max(1, pw / Math.max(1, (v1 - v0) / this.step) * 0.8);
      for (j = j0; j < j1; j++) if (b.v[j] > 0) { tot += b.v[j]; var hh = bh * b.v[j] / (mx || 1); c.fillRect(X(b.t[j]) - bw / 2, by + bh - hh, bw, hh); } c.globalAlpha = 1;
      c.fillStyle = C.fg4; c.font = '10.5px ' + C.font; c.textAlign = 'right'; c.textBaseline = 'middle'; if (mx > 0) c.fillText(Math.round(mx * 10) / 10 + ' ' + b.unit, this.L - 6, by + 4);
      notes.push('bars: ' + b.name.toLowerCase() + ', ' + Math.round(tot * 10) / 10 + ' ' + b.unit + ' in view' + (b.file ? ' (' + b.file + ')' : '')); }
    c.fillStyle = C.fg4; c.font = '10.5px ' + C.font; c.textAlign = 'center'; c.textBaseline = 'top'; var prev = -1e9, yb = H - B + 6;
    tk.ticks.forEach(function(t, i){ var x = X(t), s = tlab(t, tk.step, i === 0), tw = c.measureText(s).width; if (x - tw / 2 < prev + 8 || x + tw / 2 > W) return; c.fillText(s, x, yb); prev = x + tw / 2; });
    // event marks (field visits, downloads, logger swaps, deployment starts and ends): a dashed line through every panel and a flag at the top
    var ev = d.events || [], lastLab = -1e9; c.save(); c.font = '500 10px ' + C.font; c.textAlign = 'left'; c.textBaseline = 'top';
    ev.forEach(function(e){ if (e.t < v0 || e.t > v1) return; var x = Math.round(X(e.t)) + 0.5; c.strokeStyle = C.fg4; c.globalAlpha = 0.75; c.setLineDash([4, 3]); c.lineWidth = 1;
      c.beginPath(); c.moveTo(x, T); c.lineTo(x, H - B); c.stroke(); c.setLineDash([]); c.globalAlpha = 1; c.fillStyle = C.fg4; c.beginPath(); c.moveTo(x, T); c.lineTo(x + 7, T + 4); c.lineTo(x, T + 8); c.closePath(); c.fill();
      var lw = c.measureText(e.label).width, ly = PR ? T - 11 : T; if (x + 9 > lastLab && x + 9 + lw < self.L + pw) { c.fillText(e.label, x + 9, ly); lastLab = x + 9 + lw + 6; } });
    c.restore(); if (ev.some(function(e){ return e.t >= v0 && e.t <= v1; })) notes.push('dashed lines: field visits, downloads and logger changes');
    if (PR) { this.printFinish(c, W, H, CAP, v0, v1, C); return; }
    // the cursor: this plot's own, or the moment another plot on the page points at
    var cx = this.hx != null && this.hx >= this.L && this.hx <= this.L + pw ? this.hx : HOV && HOV.id !== this.id && HOV.t >= v0 && HOV.t <= v1 ? X(HOV.t) : null;
    if (cx != null) { c.strokeStyle = C.gh; c.beginPath(); c.moveTo(Math.round(cx) + 0.5, T); c.lineTo(Math.round(cx) + 0.5, H - B); c.stroke(); }
    this.tipShow(v0, v1, pw);
    this.drawNav(W, v0, v1, C);
    var spanTxt = this.span < 1 ? Math.round(this.span * 24) + ' hours' : Math.round(this.span * 10) / 10 + ' days', hit = false;
    var opts = WINS.map(function(x){ var on = Math.abs(self.span - x[1]) < 0.6; if (on) hit = true; return '<option value="' + x[1] + '"' + (on ? ' selected' : '') + '>' + x[0] + '</option>'; });
    opts.push('<option value="all"' + (Math.abs(this.span - this.full() / DAY) < 0.6 && !hit ? (hit = true, ' selected') : '') + '>whole record</option>');
    if (!hit) opts.unshift('<option selected>' + spanTxt + '</option>');
    this.ctl.innerHTML = '<button type="button" class="btn btn-default" data-tsv-go="prev" title="Back by most of the window">\u2039</button><button type="button" class="btn btn-default" data-tsv-go="next" title="Forward by most of the window">\u203a</button>' +
      '<select data-tsv-span title="How much of the record the plot shows">' + opts.join('') + '</select><span class="dim">' + (O.agg === 'raw' ? 'as recorded' : O.agg === 'half' ? (this.step < 1800 ? 'half-hourly means' : this.step > 1800 ? 'as recorded (records are further apart than half an hour)' : 'half-hourly') : O.agg + (d.series.some(function(s){ return s.sum; }) ? ' totals' : ' means')) +
      ' \u00b7 ' + fd(v0) + ' to ' + fd(v1) + (d.zone ? ' \u00b7 ' + esc(d.zone) : '') + '</span><span class="sp"></span><button type="button" class="btn btn-default" data-tsv-go="saveimg" title="Save the plot as an image: journal column, double column or slide">Save image</button><button type="button" class="btn btn-default" data-tsv-go="savedata" title="Save the data behind the plot as CSV">Save data</button><button type="button" class="btn btn-default" data-tsv-go="hist" title="Previous view (Backspace)"' + (this.hist.length ? '' : ' disabled') + '>\u2190 Back</button><span class="dim">drag \u2192 zooms in, \u2190 zooms out \u00b7 Shift + drag selects \u00b7 click a point \u00b7 Shift + click adds \u00b7 wheel zooms \u00b7 drag the time axis or strip to move \u00b7 double-click: whole record \u00b7 \u2190 \u2192 move the selection \u00b7 + \u2212 zoom \u00b7 Enter: look closer</span>';
    if (d.series.some(function(s){ return s.raw; }) && O.ghost) notes.push('dashed: the ghost of the raw data');
    if (d.binned) notes.push('sent as half-hourly means; every record is in the Workbench');
    this.note.textContent = notes.filter(function(x, i){ return x && notes.indexOf(x) === i; }).join(' \u00b7 ');
    this.drawSel(); this.drawAct(); this.drawRecs(); };
  // the line through the points i0..i1, broken at missing values; a spline when smooth and the points fit the width
  P.line = function(c, A, i0, i1, X, Y, pw, smooth){ var gap = A.bins ? Infinity : this.step * 2.5, i, run = [], drew = false, lt = null;
    var many = i1 - i0 > pw * 2, spl = smooth && !many && i1 - i0 <= pw / 3;
    function flush(){ if (!run.length) return; c.moveTo(run[0][0], run[0][1]);
      if (spl && run.length > 2) { for (var k = 0; k < run.length - 1; k++) { var p0 = run[Math.max(0, k - 1)], p1 = run[k], p2 = run[k + 1], p3 = run[Math.min(run.length - 1, k + 2)];
          c.bezierCurveTo(p1[0] + (p2[0] - p0[0]) / 6, p1[1] + (p2[1] - p0[1]) / 6, p2[0] - (p3[0] - p1[0]) / 6, p2[1] - (p3[1] - p1[1]) / 6, p2[0], p2[1]); } drew = true; }
      else for (var j = 1; j < run.length; j++) c.lineTo(run[j][0], run[j][1]); if (run.length === 1) c.lineTo(run[0][0] + 1, run[0][1]); run = []; }
    if (many) { var cx = null, mn, mx; for (i = i0; i < i1; i++) { var y = A.v[i]; if (y == null || (lt !== null && A.t[i] - lt > gap)) { if (cx !== null) { c.moveTo(cx, Y(mn)); c.lineTo(cx, Y(mx)); } cx = null; lt = A.t[i]; if (y == null) continue; }
        lt = A.t[i]; var x = Math.round(X(A.t[i])) + 0.5; if (x !== cx) { if (cx !== null) { c.moveTo(cx, Y(mn)); c.lineTo(cx, Y(mx)); c.lineTo(x, Y(y)); } cx = x; mn = mx = y; } else { if (y < mn) mn = y; if (y > mx) mx = y; } }
      if (cx !== null) { c.moveTo(cx, Y(mn)); c.lineTo(cx, Y(mx)); } return false; }
    for (i = i0; i < i1; i++) { var v = A.v[i]; if (v == null || !isFinite(v) || (lt !== null && A.t[i] - lt > gap)) flush(); lt = A.t[i]; if (v != null && isFinite(v)) run.push([X(A.t[i]), Y(v)]); }
    flush(); return drew; };
  P.tipShow = function(v0, v1, pw){ var tip = this.tip, d = this.d; if (this.hx == null || this.dr || this.hx < this.L || this.hx > this.L + pw) { tip.style.display = 'none'; return; }
    var t = v0 + (this.hx - this.L) / pw * (v1 - v0), self = this, rows = [], tt = null;
    d.series.forEach(function(s){ var A = agg(self, s, 'v'); if (!A.t.length) return; var i = lower(A.t, t); if (i > 0 && (i >= A.t.length || t - A.t[i - 1] < A.t[i] - t)) i--;
      if (tt === null) tt = A.t[i]; rows.push('<div>' + (s.col ? '<i class="sw" style="background:' + esc(s.col) + '"></i>' : '') + esc(s.name) + '<b>' + fv(A.v[i], s.digits) + (s.unit ? ' ' + esc(s.unit) : '') + '</b></div>'); });
    if (tt === null) { tip.style.display = 'none'; return; }
    var A0 = agg(this, d.series[0], 'v'), s0 = d.series[0], zone = d.zone ? ' (' + esc(d.zone) + ')' : '', extra = [];
    if (!A0.bins) { var j = lower(s0.t, tt - 1e-6), st = 'ok';                         // the record's status, and the logger and file it came from
      if (s0.raw && s0.raw[j] != null && s0.v[j] == null) st = 'removed'; else if (s0.fill && s0.fill[j]) st = 'filled (estimated)';
      d.issues.forEach(function(q){ if (tt >= q.t0 - 1 && tt <= q.t1 + 1 && st === 'ok') st = (q.level === 'error' ? 'error: ' : 'flagged: ') + (q.label || 'under review'); });
      d.spans.forEach(function(q){ if (tt >= q.t0 && tt <= q.t1 && st === 'ok') st = q.label || 'shaded'; });
      extra.push('<div>Status<b>' + esc(st) + '</b></div>');
      var src = null; (d.sources || []).forEach(function(x){ if (x.from <= tt) src = x; }); if (src) extra.push('<div class="dim">' + esc(src.label) + '</div>'); }
    var wv = (v1 - v0) / pw * 5; (d.events || []).forEach(function(e){ if (Math.abs(e.t - t) <= wv) extra.push('<div class="ev"><b style="margin:0">' + esc(e.label) + '</b> ' + ft(e.t) + (e.note ? ' \u00b7 ' + esc(e.note) : '') + '</div>'); });
    tip.innerHTML = '<div class="h">' + (A0.bins ? (O.agg === 'half' || O.agg === 'hourly' ? ft(tt - (O.agg === 'half' ? 900 : 1800)) : fd(tt)) + zone + ' \u00b7 ' + A0.label : ft(tt) + zone) + '</div>' + rows.join('') + extra.join('');
    tip.style.display = 'block'; var x = this.hx + 14; if (x + tip.offsetWidth > this.box.clientWidth) x = this.hx - tip.offsetWidth - 14; tip.style.left = Math.max(0, x) + 'px'; tip.style.top = '6px'; };
  P.drawNav = function(W, v0, v1, C){ var dpr = window.devicePixelRatio || 1, H = 22, c = this.nav.getContext('2d'), r = this.rng(), s = this.d.series[0], n = Math.max(2, Math.floor(W / 3));
    this.nav.width = W * dpr; this.nav.height = H * dpr; this.nav.style.height = H + 'px'; c.setTransform(dpr, 0, 0, dpr, 0, 0); c.clearRect(0, 0, W, H);
    var X = function(t){ return (t - r[0]) / (r[1] - r[0] || 1) * W; }, lo = Infinity, hi = -Infinity, M = [], i, k;
    for (k = 0; k < n; k++) { var a = lower(s.t, r[0] + (r[1] - r[0]) * k / n), b = lower(s.t, r[0] + (r[1] - r[0]) * (k + 1) / n), sm = 0, cn = 0;
      for (i = a; i < b; i += Math.max(1, Math.floor((b - a) / 40))) if (s.v[i] != null) { sm += s.v[i]; cn++; } M.push(cn ? sm / cn : null); if (cn) { lo = Math.min(lo, sm / cn); hi = Math.max(hi, sm / cn); } }
    c.fillStyle = css('--bg1', '#222'); c.fillRect(0, 0, W, H); c.strokeStyle = C.gh; c.lineWidth = 1; c.beginPath(); var pen = false;
    M.forEach(function(m, k){ if (m == null) { pen = false; return; } var x = (k + 0.5) / n * W, y = H - 3 - (m - lo) / (hi - lo || 1) * (H - 6); if (!pen) { c.moveTo(x, y); pen = true; } else c.lineTo(x, y); }); c.stroke();
    c.fillStyle = C.gh; c.globalAlpha = 0.5; this.d.spans.forEach(function(q){ if (q.kind === 'absent') hatch(c, X(q.t0), X(q.t1), 0, H, C.gh); else c.fillRect(X(q.t0), 0, Math.max(2, X(q.t1) - X(q.t0)), H); }); c.globalAlpha = 1;
    c.fillStyle = C.warn; this.d.issues.forEach(function(q){ c.fillRect(X(q.t0) - 1, 0, Math.max(2, X(q.t1) - X(q.t0) + 2), H); });
    c.fillStyle = C.fg4; (this.d.events || []).forEach(function(e){ c.fillRect(Math.round(X(e.t)), 0, 1, 6); });
    if (HOV && HOV.id !== this.id) { c.fillStyle = C.gh; c.fillRect(Math.round(X(HOV.t)), 0, 1.5, H); }
    c.fillStyle = C.line; c.globalAlpha = 0.18; c.fillRect(X(v0), 0, Math.max(3, X(v1) - X(v0)), H); c.globalAlpha = 1; c.strokeStyle = C.line; c.strokeRect(X(v0) + 0.5, 0.5, Math.max(3, X(v1) - X(v0)) - 1, H - 1); };
  // Look closer: the records around the stretch, raw and now, and the same moment in the Workbench
  P.drawRecs = function(){ var q = this.close, d = this.d; if (!q) { this.recs.innerHTML = ''; return; } var s = d.series[0], a = lower(s.t, q.t0 - 1), b = lower(s.t, q.t1 + 1), lo = Math.max(0, a - 6), hi = Math.min(s.t.length, b + 6, lo + 400), rows = [];   // at most 400 rows: every record is in the Workbench
    for (var i = lo; i < hi; i++) { var on = s.t[i] >= q.t0 - 1 && s.t[i] <= q.t1 + 1, raw = s.raw ? s.raw[i] : s.v[i], now = s.v[i];
      rows.push('<tr class="' + (on ? 'on' : '') + '"><td>' + ft(s.t[i]) + '</td><td class="num">' + (raw == null ? '<span class="dim">no record</span>' : fv(raw, s.digits)) + '</td><td class="num">' +
        (now == null ? (raw == null ? '' : '<span style="color:var(--red)">removed</span>') : fv(now, s.digits)) + '</td><td>' + (on ? 'under review' : '') + (s.fill && s.fill[i] ? ' filled' : '') + '</td></tr>'); }
    var href = this.wbHref(q.t0, q.t1);
    this.recs.innerHTML = '<div class="row1"><b>Records around ' + ft(q.t0) + (q.t1 > q.t0 ? ' to ' + ft(q.t1) : '') + '</b><span>' + (href ? '<a href="' + esc(href) + '" target="_blank" title="The same records in the Workbench, selected">Open in the Workbench at ' + ft(q.t0) + ' \u203a</a> ' : '') +
      '<button type="button" class="btn btn-default btn-xs" data-tsv-go="back">Back to 3 months</button></span></div><table><thead><tr><th>Time</th><th class="num">Raw' + (s.unit ? ', ' + esc(s.unit) : '') + '</th><th class="num">Now</th><th></th></tr></thead><tbody>' + rows.join('') + '</tbody></table>'; };

  // Save image: the view as it is, in the light print scheme, at a preset size, with a title and a caption (plot and table standards A10)
  P.saveImage = function(key){ var p = PRESETS[key], d = this.d; if (!p || !d) return; var cv = document.createElement('canvas'), lines = this.captionLines(), keepHx = this.hx;
    this.hx = null; this.print = {w: p.w, h: p.h, s: p.s, cv: cv, cap: 18 + 13 * lines.length, lines: lines}; try { this.draw(); } finally { this.print = null; this.hx = keepHx; }
    var a = document.createElement('a'); a.href = cv.toDataURL('image/png'); a.download = this.fileName(p.tag) + '.png'; document.body.appendChild(a); a.click(); a.remove(); this.draw(); };
  P.captionLines = function(){ var d = this.d, w = this.win(), s = [];
    s.push((d.site ? d.site + ', ' : '') + ft(w[0]).slice(0, 10) + ' to ' + ft(w[1]).slice(0, 10) + (d.zone ? ' (' + d.zone + ')' : '') + '. ' + (O.agg === 'raw' || (O.agg === 'half' && this.step >= 1800) ? 'As recorded' : (O.agg === 'half' ? 'Half-hourly' : O.agg.charAt(0).toUpperCase() + O.agg.slice(1)) + (d.series.some(function(x){ return x.sum; }) ? ' totals' : ' means')) + (d.level ? ', processing level ' + d.level : '') + '.');
    if (d.caption) s.push(d.caption); return s; };
  P.printFinish = function(c, W, H, CAP, v0, v1, C){ var d = this.d, P0 = this.print;
    c.fillStyle = C.fg; c.font = '600 12px ' + C.font; c.textAlign = 'left'; c.textBaseline = 'top'; c.fillText(d.title || d.series.map(function(s){ return s.name; }).join(', '), this.L, 8);
    c.fillStyle = C.fg4; c.font = '500 10.5px ' + C.font; c.textAlign = 'center'; c.fillText('Time' + (d.zone ? ' (' + d.zone + ')' : ''), this.L + (W - this.L - this.R) / 2, H - 16);
    c.textAlign = 'left'; c.fillStyle = C.fg4; c.font = '10px ' + C.font; var y = H + 8, self = this;
    P0.lines.forEach(function(l){ var words = String(l).split(' '), cur = ''; words.forEach(function(wd){ var t = cur ? cur + ' ' + wd : wd; if (c.measureText(t).width > W - self.L - self.R && cur) { c.fillText(cur, self.L, y); y += 13; cur = wd; } else cur = t; }); if (cur) { c.fillText(cur, self.L, y); y += 13; } }); };
  P.fileName = function(tag){ var d = this.d, w = this.win(), base = d.file || ('ovid_' + (d.series[0] ? d.series[0].key || d.series[0].name : 'plot'));
    return (base + '_' + ft(w[0]).slice(0, 10) + '_' + ft(w[1]).slice(0, 10) + (tag ? '_' + tag : '')).replace(/[^A-Za-z0-9_.-]+/g, '_'); };
  // Save data: the records behind the plot as CSV (UTF-8, ISO times with the zone in the column name, a QC column)
  P.saveData = function(){ var d = this.d; if (!d || !d.series.length) return; var tz = (d.zone || 'UTC').replace(/\s+/g, ''), s0 = d.series[0], head = ['timestamp_' + tz.toLowerCase()], i, k;
    d.series.forEach(function(s){ head.push(s.name + (s.unit ? ' (' + s.unit + ')' : '')); }); head.push('qc');
    var q = function(x){ x = String(x); return /[",\n]/.test(x) ? '"' + x.replace(/"/g, '""') + '"' : x; }, rows = [head.map(q).join(',')];
    for (i = 0; i < s0.t.length; i++) { var t = s0.t[i], r = [fiso(t)], qc = '';
      d.series.forEach(function(s){ var j = s === s0 ? i : lower(s.t, t - 1e-6), v = s.t[j] === t ? s.v[j] : null; r.push(v == null ? '' : v); if (s.raw && s.t[j] === t && s.raw[j] != null && v == null) qc = 'removed'; else if (s.fill && s.fill[j] && !qc) qc = 'filled'; });
      for (k = 0; k < d.issues.length && !qc; k++) if (t >= d.issues[k].t0 - 1 && t <= d.issues[k].t1 + 1) qc = 'flagged';
      r.push(qc || 'ok'); rows.push(r.map(q).join(',')); }
    var a = document.createElement('a'); a.href = URL.createObjectURL(new Blob([rows.join('\n') + '\n'], {type: 'text/csv;charset=utf-8'})); a.download = this.fileName('') + '.csv'; document.body.appendChild(a); a.click(); a.remove(); };
  P.saveMenu = function(btn){ var m = this.el.querySelector('.tsv-savem'); if (m) { m.remove(); return; } m = document.createElement('div'); m.className = 'tsv-savem';
    m.innerHTML = Object.keys(PRESETS).map(function(k){ var p = PRESETS[k]; return '<button type="button" data-tsv-save="' + k + '">' + p.name + '<small>' + p.sub + '</small></button>'; }).join('') + '<div class="dim">Light print scheme, with a caption: site, period, zone, processing.</div>';
    this.ctl.appendChild(m); m.style.left = Math.max(0, Math.min(this.ctl.clientWidth - m.offsetWidth, btn.offsetLeft)) + 'px'; m.style.top = (btn.offsetTop + btn.offsetHeight + 4) + 'px'; };
  // keys: arrows move the selection by a record (Shift extends it), + and - zoom, Enter looks closer
  P.key = function(e){ var k = e.key, q = this.sel, s = this.d && this.d.series[0]; if (!s) return false;
    if (k === '+' || k === '=') { this.mark(); this.zoom(1 / 1.5, q ? (q.t0 + q.t1) / 2 : null); return true; }
    if (k === '-' || k === '_') { this.mark(); this.zoom(1.5, q ? (q.t0 + q.t1) / 2 : null); return true; }
    if (!q) return false;
    if (k === 'Enter') { this.sel = null; this.closer(q.t0, q.t1); return true; }
    if (k !== 'ArrowLeft' && k !== 'ArrowRight') return false; var A = agg(this, s, 'v'), T0 = A.t, right = k === 'ArrowRight';
    var i0 = lower(T0, q.t0 - 1e-6), i1 = Math.max(i0, lower(T0, q.t1 + 1e-6) - 1);
    if (e.shiftKey) { if (right) i1 = Math.min(T0.length - 1, i1 + 1); else i0 = Math.max(0, i0 - 1); }
    else { var n = i1 - i0; if (right) { i1 = Math.min(T0.length - 1, i1 + 1); i0 = Math.max(0, i1 - n); } else { i0 = Math.max(0, i0 - 1); i1 = Math.min(T0.length - 1, i0 + n); } }
    this.sel = {t0: T0[i0], t1: T0[i1]}; var w = this.win(), edge = right ? this.sel.t1 : this.sel.t0; if (edge < w[0] || edge > w[1]) this.c = edge; this.draw(); return true; };

  // the same records in the Workbench, selected (null when the page gave no Workbench link)
  P.wbHref = function(t0, t1){ var d = this.d, wb = d.wb, s = d.series[0]; if (!wb || !wb.key) return null; var u = new URLSearchParams(), val = wb.value; u.set('ui', 'workbench');
    if (wb.from != null && wb.values != null) { var fr = [].concat(wb.from), vs = [].concat(wb.values); for (var k = 0; k < fr.length; k++) if (fr[k] <= t0) val = vs[k]; }   // a record made of several parts
    u.set(wb.key, val); if (wb.params) u.set('params', wb.params);
    u.set('from', fiso(t0 - 2 * DAY).slice(0, 10)); u.set('to', fiso(t1 + 2 * DAY).slice(0, 10)); u.set('t', fiso(t0)); u.set('t1', fiso(t1)); u.set('p', s.key || s.name); return '?' + u.toString(); };

  function get(id){ var el = document.getElementById(id); if (!el) return null; if (!V[id] || V[id].el !== el) V[id] = new Plot(el); return V[id]; }
  function all(){ Object.keys(V).forEach(function(k){ if (document.body.contains(V[k].el)) V[k].draw(); else delete V[k]; }); }
  // one cursor for the page: the moment under the pointer shows on every other plot, and goes to the page as ovid:cursor
  var CR = null; function cursor(src, t){ HOV = t == null ? null : {id: src.id, t: t}; if (CR) return; CR = requestAnimationFrame(function(){ CR = null;
    Object.keys(V).forEach(function(k){ if (V[k] !== src && V[k].d && document.body.contains(V[k].el)) V[k].draw(); }); $(document).trigger('ovid:cursor', [HOV ? HOV.t : null]); }); }
  function bar(){ document.querySelectorAll('[data-tsv-agg]').forEach(function(b){ b.classList.toggle('on', b.getAttribute('data-tsv-agg') === O.agg); });
    document.querySelectorAll('[data-tsv-opt]').forEach(function(x){ var k = x.getAttribute('data-tsv-opt'); if (x.type === 'color') x.value = O[k]; else x.checked = !!O[k]; }); }
  $(function(){ bar(); if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('tsv', function(m){ var p = get(m.id); if (p) { delete PEND[m.id]; p.set(m); } else { PEND[m.id] = m; wait(); } });
    Shiny.addCustomMessageHandler('tsv-close', function(m){ var p = get(m.id); if (p) p.closer(m.t0, m.t1); });
    Shiny.addCustomMessageHandler('tsv-done', function(m){ var p = get(m.id); if (p) p.done(m); }); } });
  // A plot can be sent before its card is in the page; keep it until the element appears.
  var PEND = {}, MO = null;
  function flush(){ Object.keys(PEND).forEach(function(k){ var p = get(k); if (p) { var m = PEND[k]; delete PEND[k]; p.set(m); } });
    if (!Object.keys(PEND).length && MO) { MO.disconnect(); MO = null; } }
  function wait(){ if (MO || !window.MutationObserver) return; MO = new MutationObserver(function(){ flush(); }); MO.observe(document.body, { childList: true, subtree: true }); }
  $(document).on('shiny:value', function(){ setTimeout(function(){ bar(); flush(); }, 0); });
  $(document).on('click', '[data-tsv-agg]', function(e){ e.preventDefault(); O.agg = this.getAttribute('data-tsv-agg'); save(); bar(); all(); });
  $(document).on('change input', '[data-tsv-opt]', function(){ var k = this.getAttribute('data-tsv-opt'); O[k] = this.type === 'color' ? this.value : this.checked; save(); all(); });
  $(document).on('click', '[data-tsv-go]', function(e){ e.preventDefault(); var el = this.closest('.tsv'), p = el && V[el.id]; if (!p) return; var a = this.getAttribute('data-tsv-go'), w = p.win();
    if (a === 'prev') p.c -= (w[1] - w[0]) * 0.75; else if (a === 'next') p.c += (w[1] - w[0]) * 0.75; else if (a === 'back') return p.back();
    else if (a === 'selclear') return p.clearSel(); else if (a === 'selcloser' && p.sel) { var q = p.sel; p.sel = null; return p.closer(q.t0, q.t1); }
    else if (a === 'selzoom') return p.zoomSel(); else if (a === 'hist') return p.goBack();
    else if (a === 'saveimg') return p.saveMenu(this); else if (a === 'savedata') return p.saveData(); p.draw(); });
  $(document).on('click', '[data-tsv-act]', function(e){ e.preventDefault(); var el = this.closest('.tsv'), p = el && V[el.id]; if (p) p.doAct(this.getAttribute('data-tsv-act')); });
  $(document).on('input', '.tsv-act [data-tsv-f]', function(){ this.classList.remove('need'); });
  // Undo on the bar after an action (or Ctrl/Cmd+Z while it shows): input <id>_undo
  function undo(b){ var el = b.closest('.tsv'); b.hidden = true; if (el && window.Shiny && Shiny.setInputValue) Shiny.setInputValue(el.id + '_undo', Date.now(), {priority: 'event'}); }
  $(document).on('click', '[data-tsv-undo]', function(e){ e.preventDefault(); var b = this.closest('.tsv-done'); if (this.getAttribute('data-tsv-undo') === '1') undo(b); else b.hidden = true; });
  $(document).on('click', '[data-tsv-save]', function(e){ e.preventDefault(); var el = this.closest('.tsv'), p = el && V[el.id], m = this.closest('.tsv-savem'); if (m) m.remove(); if (p) p.saveImage(this.getAttribute('data-tsv-save')); });
  $(document).on('mousedown', function(e){ if (!$(e.target).closest('.tsv-savem,[data-tsv-go=saveimg]').length) $('.tsv-savem').remove(); });
  // Esc clears, Backspace goes back a view, Space held down makes a drag move the plot (as in drawing programs)
  $(document).on('keydown', function(e){ if ($(e.target).is('input,textarea,select,[contenteditable]')) return;
    if ((e.metaKey || e.ctrlKey) && !e.shiftKey && (e.key === 'z' || e.key === 'Z')) { var ub = [].filter.call(document.querySelectorAll('.tsv-done'), function(b){ return !b.hidden && b.querySelector('[data-tsv-undo="1"]'); })[0];
      if (ub) { e.preventDefault(); undo(ub); } return; }
    if (e.key === 'Escape') Object.keys(V).forEach(function(k){ if (V[k].sel) V[k].clearSel(); });
    else if (e.key === ' ' && Object.keys(V).length) { SPACE = true; e.preventDefault(); }
    else if (e.key === 'Backspace') { var h = document.querySelector('.tsv:hover'), p = h && V[h.id]; if (p && p.goBack()) e.preventDefault(); }
    else if (!e.metaKey && !e.ctrlKey && !e.altKey && /^(ArrowLeft|ArrowRight|Enter|\+|=|-|_)$/.test(e.key)) { var hv = document.querySelector('.tsv:hover'), p2 = (hv && V[hv.id]) || (ACT && document.body.contains(ACT.el) ? ACT : null);
      if (p2 && p2.key(e)) e.preventDefault(); } });
  $(document).on('keyup', function(e){ if (e.key === ' ') SPACE = false; });
  $(document).on('change', '[data-tsv-span]', function(){ var el = this.closest('.tsv'), p = el && V[el.id]; if (!p) return; p.span = this.value === 'all' ? p.full() / DAY : +this.value || p.span; p.draw(); });
  $(document).on('click', '[data-tsv-close]', function(e){ e.preventDefault(); var p = get(this.getAttribute('data-tsv-close')); if (p && p.d) p.closer(+this.getAttribute('data-t0'), +this.getAttribute('data-t1')); });
  $(document).on('shown.bs.tab', function(){ setTimeout(all, 30); });
  window.ovidTsv = {get: get, opts: O};
})();

