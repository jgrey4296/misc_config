# -*- mode: snippet -*-
# name: gradle.command
# uuid: gradle.command
# key:  gradle.command
# group :
# --
task $1(type:Exec) {
     description ""
     workingDir '.'
     environment []
     executable '$2'
     args '$3'

     //store the output instead of printing to the console:
     standardOutput = new ByteArrayOutputStream()
     //extension method $1.output() can be used to obtain the output:
     ext.output = {
        return standardOutput.toString()
    }
}
