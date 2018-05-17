var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var webpack = require('webpack');

var paths = {
  dest: 'dist',
  source: './src/**/*.elm',
  staticAssets: './*.css',
  wpAssets: ['./src/**/*.scss', './src/**/*.js'],
  staticPages: ['./src/index.html', './src/background.js']
};

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
  return gulp.src('./*.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest(paths.dest + '/'));
});

gulp.task('copy-static', function() {
  gulp.src(paths.staticPages)
    .pipe(gulp.dest('./dist/'));
})

gulp.task('elm-bundle', ['elm-init'], function(){
  return gulp.src([paths.source])
    .pipe(plumber())
    .pipe(elm.bundle('bundle.js'))
    .pipe(gulp.dest(paths.dest + '/'));
});

gulp.task('build-webpack', function() {
  webpack(require('./webpack.config.js')).run((_) => console.log('Webpack complete'));
});

gulp.task('watch', function() {
  gulp.watch([paths.source], ['elm-bundle']);
  gulp.watch(paths.staticAssets, ['elm-bundle']);
  gulp.watch(paths.wpAssets, ['build-webpack']);
});

gulp.task('dev', ['elm-bundle', 'build-webpack', 'watch']);
