import { defineConfig } from 'vite';
export default defineConfig(({command, mode}) => ({
    base: './',
    build: {
        assetsInclude: ['**/*.xsl']
    }
}));
