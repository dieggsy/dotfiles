module.exports = {
	config: {
		// default font size in pixels for all tabs
		fontSize: 10,

		// font family with optional fallbacks
		fontFamily: 'Input, Menlo, "DejaVu Sans Mono", "Lucida Console", monospace',

		// terminal cursor background color and opacity (hex, rgb, hsl, hsv, hwb or cmyk)
		cursorColor: '#e6e5e5',

		// `BEAM` for |, `UNDERLINE` for _, `BLOCK` for █
		cursorShape: 'BEAM',

		// color of the text
		foregroundColor: '#fdf4c1',

		// terminal background color
		backgroundColor: '#282828',

		// border color (window, tabs)
		borderColor: '#282828',

		// custom css to embed in the main window
		css: `
    .tab_active {
        color: #fdf4c1 !important;
        // border-bottom: 1px solid #B8BB26 !important;
        border-bottom: 1px solid #d3869b !important;
    }

    .tab_tab{
        color: #fdf4c1 !important;
    }
    `,

		// custom css to embed in the terminal window
		termCSS: `\
    .cursor-node{
        border: none !important;
        border-left: 1px solid #fdf4c1 !important;
    }`,
		// custom padding (css format, i.e.: `top right bottom left`)
		padding: '5px 10px 0px 10px',

		// the full list. if you're going to provide the full color palette,
		// including the 6 x 6 color cubes and the grayscale map, just provide
		// an array here instead of a color map object
		colors: {
			black: '#1a1a1a',
			red: '#9d0006',
			green: '#79740e',
			yellow: '#b57614',
			blue: '#076678',
			magenta: '#8f3f71',
			cyan: '#00a7af',
			white: '#bdae93',
			lightBlack: '#686868',
			lightRed: '#fb4933',
			lightGreen: '#b8bb26',
			lightYellow: '#fabd2f',
			lightBlue: '#83a598',
			lightMagenta: '#d3869b',
			lightCyan: '#3fd7e5',
			lightWhite: '#fdf4c1'
		},

		// the shell to run when spawning a new session (i.e. /usr/local/bin/fish)
		// if left empty, your system's login shell will be used by default
		shell: '',

		// for setting shell arguments (i.e. for using interactive shellArgs: ['-i'])
		// by default ['--login'] will be used
		shellArgs: ['--login'],

		// for environment variables
		env: {},

		// set to false for no bell
		bell: false,

		// if true, selected text will automatically be copied to the clipboard
		copyOnSelect: false

		// URL to custom bell
		// bellSoundURL: 'http://example.com/bell.mp3',

		// for advanced config flags please refer to https://hyper.is/#cfg
		// overlay: {
		// 	alwaysOnTop: true,
		// 	animate: true,
		// 	hasShadow: false,
		// 	hideDock: false,
		// 	hideOnBlur: false,
		// 	hotkeys: ['Command+Space'],
		// 	position: 'top',
		// 	primaryDisplay: false,
		// 	resizable: true,
		// 	startAlone: false,
		// 	startup: false,
		// 	size: 0.4,
		// 	tray: true,
		// 	unique: false
		// },
	},

	// a list of plugins to fetch and install from npm
	// format: [@org/]project[#version]
	// examples:
	//   `hyperpower`
	//   `@company/project`
	//   `project#1.0.1`
	plugins: [
		'hypercwd',
		'hyperterm-blink'
	],

	// in development, you can create a directory under
	// `~/.hyper_plugins/local/` and include it here
	// to load it and avoid it being `npm install`ed
	localPlugins: []
};
