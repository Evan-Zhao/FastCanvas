// Backend and endpoint details
const host     = 'http://127.0.0.1:8080'
const endpoint = '/courses'
// Retry configuration
let maxNoOfAttempts        = 3,
    waitTimeBetweenAttempt = 2000

makeCourseDiv = function(index, courseName) {
  return `<div class="nav-div course-${index}">
  <button type="button" id="button-${index}" class="nav-button"> ${courseName}</button>
  </div>`
}

let _fetchCourseList = function(waitTime, maxAttempts, currentAttemptNo) {
  $.getJSON(host + endpoint, function(courses) {
    if (courses.Left != undefined) {
      $('#status').html(`Server responded, but an error occurred: ${courses.Left}`)
    }
    else {
      // Construct the course list HTML output
      let output = "";
      for (let i in courses.Right) {
        let course = courses.Right[i]
        output += makeCourseDiv(i, course.name)
      }
      $('#nav').append(output)
      $('#status').html(``)
    }
  }).fail(function() {
    $('#status').html(`Lost connect from server`)
    // Keep trying until we get an answer or reach the maximum number of retries
    if (currentAttemptNo < maxAttempts) {
      setTimeout(function() {
        _fetchCourseList(waitTime, maxAttempts, currentAttemptNo+1)
      }, waitTime)
    }
  })
}

// Convenience function for _fetchCourseList
let fetchCourseList = function(waitTimeBetweenAttempt, maxNoOfAttempts) {
  _fetchCourseList(waitTimeBetweenAttempt, maxNoOfAttempts, 1)
}

// Start trying to fetch the course list
fetchCourseList(waitTimeBetweenAttempt, maxNoOfAttempts)
