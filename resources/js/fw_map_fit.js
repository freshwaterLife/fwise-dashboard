// fw_map_fit.js - one world, filling the map's width, and never a second copy.
//
// The client panned left and right to "the full world view" and saw a world
// with no eradications on it: the tiles repeated round the globe and the
// markers did not (client, 29 Sept 2026). The tiles no longer wrap and the
// pan is held inside +/-180 (see fw_leaflet() in R/maps.R). So every map sets
// its own zoom-out limit to the zoom at which one world is exactly as wide as
// the map, on load and on every resize.
//
// A FRACTIONAL ZOOM, NOT THE NEXT WHOLE ONE (client, 30 Sept 2026: "not all
// of the map is visible when opened"). Rounding up left the world up to twice
// the map's width, and so twice its height too, with the top and bottom of it
// off screen. The map is now the shape of the band it shows (fw_map_aspect()
// in R/maps.R), so at exactly one world's width the band fills it both ways.
//
// WHOLE STEPS EVERYWHERE BUT THE LIMIT. Leaflet rounds every zoom to a whole
// step (zoomSnap 1) BEFORE clamping it to minZoom, so a fit or a zoom-out to
// a limit of 1.87 rounded to 2 and cut the edges of the band off again. Each
// map's _limitZoom therefore returns the limit itself for any request at or
// below it, and snaps as before above it: the + and - buttons, the wheel and
// every fit still land on whole zooms, except the last step out.
//
// A Leaflet init hook rather than an htmlwidgets onRender, so the card script
// stays each widget's only render hook, and so the same file serves the app
// and the saved report without a Shiny session behind it.
(function () {
  if (!window.L || !L.Map || L.Map.fwFitWidth) return;
  L.Map.fwFitWidth = true;

  // THE WHOLE MAP IN ONE GROUP (client, 30 Sept 2026). Leaflet.markercluster
  // builds its tree once, down to Math.floor(map.getMinZoom()) at the time,
  // and parks every marker in one top-level group at the zoom below that. It
  // never hears that minZoom moved. So a map that got narrower after its
  // markers went on - a window resized, a scrollbar appearing - could zoom out
  // onto that top group and show all 900 attempts as a single ring. Clearing
  // and re-adding the markers rebuilds the tree from the current minZoom. The
  // card script's wiring is keyed on the marker (fwWired), so it is not
  // doubled by the re-add.
  function reclusterBelow(map) {
    if (!L.MarkerClusterGroup) return;
    var floor = Math.floor(map.getMinZoom());
    map.eachLayer(function (layer) {
      if (!(layer instanceof L.MarkerClusterGroup) || !layer._topClusterLevel) return;
      if (layer._topClusterLevel._zoom < floor) return;
      var markers = layer.getLayers();
      layer.clearLayers();
      layer.addLayers(markers);
    });
  }

  // A FRACTIONAL LIMIT NEEDS ONE FIX IN THE CLUSTER PLUGIN. Leaflet.markercluster
  // floors map.getMinZoom() nearly everywhere, but three of its walks down the
  // tree start from the raw value - _recursivelyAddChildrenToMap from
  // getMinZoom() - 1 - and at a limit of 2.04 that starts at 1.04, above the
  // top level at zoom 1, which is where every marker that never groups lives.
  // Every single dot vanished and only the counted rings were drawn. Flooring
  // the start of the walk makes those three agree with the rest of the plugin;
  // for a whole-number limit it changes nothing. Patched on the way into
  // addLayer, not here or at map creation: on a Shiny page the map is created
  // when the page binds, and the plugin only arrives later with the markers.
  function patchCluster() {
    if (!L.MarkerCluster || L.MarkerCluster.fwFloored) return;
    L.MarkerCluster.fwFloored = true;
    var walk = L.MarkerCluster.prototype._recursively;
    L.MarkerCluster.prototype._recursively = function (bounds, from, to, a, b) {
      return walk.call(this, bounds, Math.floor(from), to, a, b);
    };
  }

  var addLayer = L.Map.prototype.addLayer;
  L.Map.prototype.addLayer = function (layer) {
    patchCluster();
    return addLayer.call(this, layer);
  };

  L.Map.addInitHook(function () {
    var map = this;
    var floor = map.options.minZoom || 0;
    var limitZoom = map._limitZoom;
    map._limitZoom = function (zoom) {
      var min = this.getMinZoom();
      return zoom <= min + 1e-6 ? min : limitZoom.call(this, zoom);
    };
    function fit() {
      var w = map.getSize().x;
      // A map in a hidden tab has no width yet; it is fitted on the resize
      // that showing it fires.
      if (!w) return;
      var z = Math.max(floor, Math.log(w / 256) / Math.LN2);
      var was = map.getMinZoom();
      // A map at its limit stays at it when the limit moves, so the whole band
      // stays in view through a resize; one zoomed in is left where it is.
      var follow = map.getZoom() <= was + 1e-6 || map.getZoom() < z;
      if (was !== z) {
        map.setMinZoom(z);
        reclusterBelow(map);
      }
      if (follow) map.setZoom(z, { animate: false });
    }
    map.whenReady(fit);
    map.on('resize', fit);
  });
})();
