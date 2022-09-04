import { defineConfig } from 'vite';
export default defineConfig(({command, mode}) => ({
    build: {
        assetsInclude: ['**/*.xsl']
    }
}));
