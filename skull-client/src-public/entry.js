var app = require('./../src/Main.purs')

if(!httpUrlRoot) {
  throw 'Please define httpUrlRoot'
}

var main = app.main(httpUrlRoot)

if (module.hot) {
  module.hot.dispose(function () {
    window.document.body.innerHTML = ""
  })
  module.hot.accept()
  main(true)()
} else {
  main(false)()
}
