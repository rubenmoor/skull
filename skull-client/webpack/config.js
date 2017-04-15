var path = require('path')
var webpack = require('webpack')
var ExtendedDefinePlugin = require('extended-define-webpack-plugin')
var HtmlWebpackPlugin = require('html-webpack-plugin')

module.exports = {
  entry: [
    'webpack-hot-middleware/client?reload=true',
    path.join(__dirname, '../src-public/entry.js')
  ],
  output: {
    path: path.resolve('./public'),
    filename: 'bundle.js',
    publicPath: ''
  },
  module: {
    rules: [
      {
        test: /\.purs$/,
        exclude: /node_modules/,
        loader: 'purs-loader',
        query: {
          bundle: false,
          psc: 'psa',
          pscIde: true,
          pscArgs: {
            sourceMaps: true
          }
        }
      }
    ]
  },
  plugins: [
    new webpack.SourceMapDevToolPlugin({
      filename: '[file].map',
      moduleFilenameTemplate: '[absolute-resource-path]',
      fallbackModuleFilenameTemplate: '[absolute-resource-path]'
    }),
    new webpack.HotModuleReplacementPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new HtmlWebpackPlugin({
      template: './src-public/index.html'
    }),
    new ExtendedDefinePlugin({
      httpUrlRoot: process.env.HTTP_URL_ROOT
    })
  ],
  resolveLoader: {
    modules: [
      path.join(__dirname, '../node_modules')
    ]
  },
  resolve: {
    modules: [
      'node_modules',
      'bower_components'
    ],
    extensions: ['.js', '.purs']
  }
}
