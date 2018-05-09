var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');

var paths = {
	dest: 'dist',
	source: './src/**/*.elm',
	staticAssets: './*.css'
};

function handleError(err) {
	console.log(err.toString());
	this.emit('end');
}

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('./*.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest(paths.dest + '/'));
});

gulp.task('elm-bundle', ['elm-init'], function(){
  return gulp.src([paths.source])
    .pipe(plumber())
    .pipe(elm.bundle('bundle.js'))
    .pipe(gulp.dest(paths.dest + '/'));
});

gulp.task('watch', function() {
	gulp.watch([paths.source], ['elm-bundle']);
	gulp.watch(paths.staticAssets, ['elm-bundle']);
});

gulp.task('dev', ['elm-bundle', 'watch']);
