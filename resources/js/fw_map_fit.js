// fw_map_fit.js - one world, filling the map's width, and never a second copy.
//
// The client panned left and right to "the full world view" and saw a world
// with no eradications on it: the tiles repeated round the globe and the
// markers did not (client, 29 Sept 2026). The tiles no longer wrap and the
// pan is held inside +/-180 (see fw_leaflet() in R/maps.R). What is left is a
// wide screen, where at zoom 2 the world (1024 px) is narrower than the map
// and grey shows at either side. So every map raises its own zoom-out limit
// to the smallest whole zoom at which one world is at least as wide as the
// map, on load and on every resize.
//
// A Leaflet init hook rather than an htmlwidgets onRender, so the card script
// stays each widget's only render hook, and so the same file serves the app
// and the saved report without a Shiny session behind it.
(function () {
  if (!window.L || !L.Map || L.Map.fwFitWidth) return;
  L.Map.fwFitWidth = true;
  L.Map.addInitHook(function () {
    var map = this;
    var floor = map.options.minZoom || 0;
    function fit() {
      var w = map.getSize().x;
      // A map in a hidden tab has no width yet; it is fitted on the resize
      // that showing it fires.
      if (!w) return;
      var z = Math.max(floor, Math.ceil(Math.log(w / 256) / Math.LN2));
      if (map.getMinZoom() !== z) map.setMinZoom(z);
      if (map.getZoom() < z) map.setZoom(z, { animate: false });
    }
    map.whenReady(fit);
    map.on('resize', fit);
  });
})();
