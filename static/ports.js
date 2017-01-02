var requestAnimationFrame = typeof window.requestAnimationFrame !== 'undefined'
  ? window.requestAnimationFrame
  : function (callback) { setTimeout(function () { callback() }, 0) }

// eslint-disable-next-line no-unused-vars
function setupLocalStoragePort (app) {
  app.ports.writeToLocalStorage.subscribe(function (options) {
    if (options.value !== null) {
      window.localStorage.setItem(options.key, JSON.stringify(options.value, null, 2))
    } else {
      window.localStorage.removeItem(options.key)
    }
  })
}

// eslint-disable-next-line no-unused-vars
function setupWaterfallPort (app) {
  function yFormatter (n) {
    n = Math.round(n)
    var result = n
    if (Math.abs(n) > 999) {
      result = Math.round(n / 1000) + 'K'
    }
    // TODO Do not hardcode currency
    return result + ' €'
  }
  function renderWaterfall (options) {
    var waterfallSelector = '#waterfall'
    var waterfallElement = document.querySelector(waterfallSelector)
    if (waterfallElement) {
      var svgElement = waterfallElement.querySelector('svg')
      if (svgElement) {
        svgElement.parentNode.removeChild(svgElement)
      }
      var defaultOptions = {
        elementSelector: waterfallSelector,
        viewPort: {
          height: waterfallElement.clientWidth * 0.5,
          width: waterfallElement.clientWidth
        },
        yFormatter: yFormatter
      }
      waterfallChart(Object.assign(defaultOptions, options)) // eslint-disable-line no-undef
    }
  }
  app.ports.renderWaterfall.subscribe(function (options) {
    window.onresize = function ( /* event */) {
      renderWaterfall(options)
    }
    // Use requestAnimationFrame to render the chart after the Elm view is rendered.
    requestAnimationFrame(function () {
      renderWaterfall(options)
    })
  })
}
