// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    /* ... */
  },
  plugins: [
    ['snowpack-plugin-unicode', {
        inputExtensions: ['.xsl'],
        inputEncoding: 'utf-8'
    }],
/*    [
        '@snowpack/plugin-webpack',
        {},
    ], */
  ],
  packageOptions: {
    /* ... */
  },
  devOptions: {
    /* ... */
  },
  buildOptions: {
    /* ... */
  },
  
  optimize: {
      bundle: true,
      minify: true,
      target: 'es2018',
      treeshake: true
  },
  
  exclude: [
    '**/chaff/**',
    '**/node_modules/**',
    '**/CHANGELOG',
    '**/README.md',
    '**/*.lock',
    '**/package.json',
    '**/snowpack.config.js',
  ],
};
