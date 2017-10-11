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
          psc: 'node_modules/.bin/psa',
          pscIde: true,
          pscArgs: {
            sourceMaps: true
          }
        }
      },
      {
        test: /\.css/,
        use: [
          'style-loader',
          'css-loader?importLoaders=1',
          {
            loader: 'postcss-loader',
            options: {
              plugins: function () {
                return [
                  require('autoprefixer'),
                  require('postcss-calc'),
                  require('postcss-color-function'),
                  require('postcss-custom-properties'),
                  require('postcss-import'),
                  require('postcss-nested'),
                  require('postcss-simple-vars')
                ]
              }
            }
          }
        ]
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
