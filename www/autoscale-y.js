/*
 * autoscale-y.js
 *
 * Two behaviours:
 *   1. Preserve x-axis range when the transform toggle changes.
 *   2. Auto-scale y-axis to the visible x-range on every x change or re-render.
 *
 * Handles line charts and stacked relative bar charts (decomp).
 */

(function () {
  "use strict";

  // Chart id → x-range snapshot taken just before a transform click
  var pendingXRange = {};

  // ── 1. Save x-range on transform toggle click ─────────────────────────────

  document.addEventListener("click", function (e) {
    var btn = e.target.closest(".tfm-btn");
    if (!btn) return;
    var chartId = btn.getAttribute("data-tfm-input").replace(/_tfm$/, "");
    var el = document.getElementById(chartId);
    if (el && el.layout && el.layout.xaxis && el.layout.xaxis.range) {
      pendingXRange[chartId] = el.layout.xaxis.range.slice();
    }
  });

  // ── 2. After Shiny delivers a new chart ───────────────────────────────────

  $(document).on("shiny:value", function (e) {
    var id    = e.name;
    var saved = pendingXRange[id];
    delete pendingXRange[id];          // consume once

    setTimeout(function () {
      var el = document.getElementById(id);
      if (!el || typeof el.on !== "function") return;

      // Re-register handler every render.
      // Plotly.newPlot clears listeners registered with el.on(), so we must
      // always remove-then-add — the _autoYAttached guard is NOT sufficient.
      if (el._autoYHandler) {
        try { el.removeListener("plotly_relayout", el._autoYHandler); } catch (_) {}
      }
      el._autoYHandler = function (ev) {
        if (isXChange(ev)) scaleYToX(el);
      };
      el.on("plotly_relayout", el._autoYHandler);

      if (saved) {
        // Transform toggle: restore x-range, THEN scale y via the Promise
        // returned by Plotly.relayout (resolves once the relayout is painted).
        var p = Plotly.relayout(el, {
          "xaxis.range[0]": saved[0],
          "xaxis.range[1]": saved[1]
        });
        if (p && typeof p.then === "function") {
          p.then(function () { scaleYToX(el); });
        } else {
          setTimeout(function () { scaleYToX(el); }, 60);
        }
      } else {
        // Data refresh or first load: x already set to 20Y default by R layout
        scaleYToX(el);
      }
    }, 150);
  });

  // ── helpers ───────────────────────────────────────────────────────────────

  function isXChange(ev) {
    return (
      ev.hasOwnProperty("xaxis.range[0]") ||
      ev.hasOwnProperty("xaxis.range")    ||
      ev["xaxis.autorange"] === true
    );
  }

  function scaleYToX(el) {
    var layout = el.layout;
    if (!layout || !layout.xaxis || !layout.xaxis.range) return;

    var x0 = +new Date(layout.xaxis.range[0]);
    var x1 = +new Date(layout.xaxis.range[1]);
    if (!isFinite(x0) || !isFinite(x1)) return;

    var lo, hi;

    if (layout.barmode === "relative") {
      // Stacked bar: accumulate positive and negative stacks per date bucket,
      // then include the aggregate-growth overlay line.
      var stacks = {};
      el.data.forEach(function (trace) {
        if (trace.visible === false || trace.visible === "legendonly") return;
        if (!trace.x || !trace.y) return;
        if (trace.type === "bar") {
          for (var i = 0; i < trace.x.length; i++) {
            var t = +new Date(trace.x[i]);
            if (t < x0 || t > x1) continue;
            var v = +trace.y[i];
            if (!isFinite(v)) continue;
            if (!stacks[t]) stacks[t] = { pos: 0, neg: 0 };
            if (v >= 0) stacks[t].pos += v; else stacks[t].neg += v;
          }
        } else {
          for (var i = 0; i < trace.x.length; i++) {
            var t = +new Date(trace.x[i]);
            if (t < x0 || t > x1) continue;
            var v = +trace.y[i];
            if (!isFinite(v)) continue;
            lo = (lo === undefined) ? v : Math.min(lo, v);
            hi = (hi === undefined) ? v : Math.max(hi, v);
          }
        }
      });
      Object.values(stacks).forEach(function (s) {
        lo = (lo === undefined) ? s.neg : Math.min(lo, s.neg);
        hi = (hi === undefined) ? s.pos : Math.max(hi, s.pos);
      });

    } else {
      // Line / scatter
      el.data.forEach(function (trace) {
        if (trace.visible === false || trace.visible === "legendonly") return;
        if (!trace.x || !trace.y) return;
        for (var i = 0; i < trace.x.length; i++) {
          var t = +new Date(trace.x[i]);
          if (t < x0 || t > x1) continue;
          var v = +trace.y[i];
          if (!isFinite(v)) continue;
          lo = (lo === undefined) ? v : Math.min(lo, v);
          hi = (hi === undefined) ? v : Math.max(hi, v);
        }
      });
    }

    if (lo === undefined || hi === undefined || lo === hi) return;

    var pad = (hi - lo) * 0.08;
    Plotly.relayout(el, { "yaxis.range": [lo - pad, hi + pad] });
  }

})();
