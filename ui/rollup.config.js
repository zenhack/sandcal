import node_resolve from 'rollup-plugin-node-resolve'

export default {
	input: './src/entry.js',
	output: {
		file: './bundle.js',
		format: 'iife',
		name: 'spread',
	},
	plugins: [
		node_resolve({module: true, browser: true}),
	],
}
