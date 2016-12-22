const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js'
, devtool: 'sourcemap'
, plugins: [
    new HtmlWebpackPlugin({
      title: "tr"
    , template: "./src/index.html"
    , inject: "body"
    })
  // , new webpack.optimize.UglifyJsPlugin({
  //     compressor: {
  //       warnings: false
  //     }
  //   })
  , new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ]
, output: {
    filename: 'bundle.js'
  , path: './static'
  , publicPath: '/static'
  }
, module: {
    rules: [{
        test: /\.js$/
      , exclude: /node_modules/
      , loader: 'babel-loader'
      , options: { 
          "presets": ["es2015", "stage-0"]
        , "plugins": ["babel-plugin-syntax-jsx", "babel-plugin-inferno"]
        }
      }
    ]
  }
}

