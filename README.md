# FastCanvas

A small thing just for automating Canvas

## Usage

After deployment, trigger the executable in cmd, and visit *localhost:8080/sync* and wait.

The progress information will be displayed on cmd window in the form of JSON (temporarily), and the final summary will be shown on the webpage.

## Design

The program will produce two threads at runtime:

1. Observer thread (main thread) forks the working thread, 

    feed it a cross-thread channel for passing progress information, 
  
    and enter a loop observing.
  
2. Working thread is warped in a monad stack, providing *Global Variables*, *Logger*, *Communicating channel*, *Exception handling* and *IO*.

The outcome: 

* final result is fedback as the content of webpage.

* progress information (as the operations may take long) are fed to frontend through server-push (thread #1 will do it).

## TODO

* [ ] Make exceptions friendly to frontend by using a custom `show` function improving readability of exceptions.

* [x] Make `courses` also an endpoint, so that frontend can get course list.

* [ ] Add git-like version control system; it can be a much simplified version.

* [ ] Add remapping of files: a list of `filter` rules selecting files, and a list of `remapping` rules to modify and move them. 

    For example, I want to move all files satisfying regex "lecture\d*_ve216_ch\d*" to "Handout\$2\Lecture$1".
    
* [ ] Have a frontend. It could well be Electron, or others.
