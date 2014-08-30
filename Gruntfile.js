module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
            srcFiles: [
                "bower_components/**/src/**/*.purs",
                "libs/**/src/**/*.purs",
                "src/**/*.purs"
            ],

            psc: {
                options: {
                },
                all: {
                    src: ["<%=srcFiles%>"],
                    dest: "dist/Main.js"
                }
            },

            dotPsci: ["<%=srcFiles%>"]
    });

    grunt.loadNpmTasks("grunt-purescript");
    grunt.registerTask("default", ["psc:all", "dotPsci"]);
};

