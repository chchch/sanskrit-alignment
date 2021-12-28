// Snowpack Configuration File
// See all supported options: https://www.snowpack.dev/reference/configuration

/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    /* ... */
  },
  plugins: [
/*    [
        '@snowpack/plugin-webpack',
        {
            },
        },
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
