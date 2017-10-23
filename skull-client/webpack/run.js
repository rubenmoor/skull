var webpack = require('webpack')
var express = require('express')
var proxy = require('express-http-proxy')

var config = require('./config.js')
var compiler = webpack(config)
var app = express()

app
  .use('/proxy', proxy('localhost:3000'))
  .use(require('connect-history-api-fallback')())
  .use(require('webpack-dev-middleware')(compiler, {
    publicPath: config.output.publicPath,
    stats: {
      hash: false,
      timings: false,
      version: false,
      assets: false,
      errors: true,
      colors: false,
      chunks: false,
      children: false,
      cached: false,
      modules: false,
      chunkModules: false
    }
  }))
  .use(require('webpack-hot-middleware')(compiler))
  .listen(3001);
