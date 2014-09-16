;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end

#include <time.h>
#include <stdint.h>
 
 /* Assumes time_t is of 32-bit integer type. */
 int32_t current_system_seconds ()
 {
   return (int32_t) (time (NULL));
 }
 
 int32_t systemtime_to_seconds (struct tm *t)
 {
   return (int32_t) (mktime (t));
 }

 struct tm *seconds_to_systemtime (int32_t t)
 {
   time_t tt = (time_t) t;
   return localtime(&tt);
 }
 
 struct tm *current_systemtime ()
 {
   time_t timer = time (NULL);
  return localtime(&timer);
 }
 
 int systemtime_hour (struct tm *t)
 {
   return t->tm_hour;
 }

 void systemtime_set_hour (struct tm *t, int h)
 {
   t->tm_hour = h;
 }
 
 int systemtime_minute (struct tm *t)
 {
   return t->tm_min;
 }

 void systemtime_set_minute (struct tm *t, int m)
 {
   t->tm_min = m;
 }
 
 int systemtime_second (struct tm *t)
 {
  return t->tm_sec;
 }

 void systemtime_set_second (struct tm *t, int s)
 {
   t->tm_sec = s;
 }
 
 int systemtime_day_of_month (struct tm *t)
 {
   return t->tm_mday;
 }

 void systemtime_set_day_of_month (struct tm *t, int d)
 {
   t->tm_mday = d;
 }
 
 int systemtime_month (struct tm *t)
 {
   return t->tm_mon;
 }

 void systemtime_set_month (struct tm *t, int m)
 {
   t->tm_mon = m;
 }
 
 int systemtime_year (struct tm *t)
 {
   return (t->tm_year + 1900);
 }

 void systemtime_set_year (struct tm *t, int y)
 {
   t->tm_year = y - 1900;
 }
 
 int systemtime_day_of_week (struct tm *t)
 {
   return t->tm_wday;
 }

 void systemtime_set_day_of_week (struct tm *t, int d)
 {
   t->tm_wday = d;
 }
 
 int systemtime_day_of_year (struct tm *t)
 {
   return t->tm_yday;
 }

 void systemtime_set_day_of_year (struct tm *t, int d)
 {
   t->tm_yday = d;
 }
 
 /* Daylight Saving Time flag. */
 int systemtime_is_dst (struct tm *t)
 {
   return t->tm_isdst;
 }

 void systemtime_set_is_dst (struct tm *t, int d)
 {
   t->tm_isdst = d;
 }
 
c-declare-end
)

(c-define-type tm* (pointer (struct "tm")))

(define current-time-seconds (c-lambda () int32 "current_system_seconds"))
(define systemtime-to-seconds (c-lambda (tm*) int32 "systemtime_to_seconds"))
(define seconds-to-systemtime (c-lambda (int32) tm* "seconds_to_systemtime"))
(define current-systemtime (c-lambda () tm* "current_systemtime"))
(define systemtime-hour (c-lambda (tm*) int "systemtime_hour"))
(define systemtime-set-hour (c-lambda (tm* int) void "systemtime_set_hour"))
(define systemtime-minute (c-lambda (tm*) int "systemtime_minute"))
(define systemtime-set-minute (c-lambda (tm* int) void "systemtime_set_minute"))
(define systemtime-second (c-lambda (tm*) int "systemtime_second"))
(define systemtime-set-second (c-lambda (tm* int) void "systemtime_set_second"))
(define systemtime-year (c-lambda (tm*) int "systemtime_year"))
(define systemtime-set-year (c-lambda (tm* int) void "systemtime_set_year"))
(define systemtime-month (c-lambda (tm*) int "systemtime_month"))
(define systemtime-set-month (c-lambda (tm* int) void "systemtime_set_month"))
(define systemtime-day-of-month (c-lambda (tm*) int "systemtime_day_of_month"))
(define systemtime-set-day-of-month (c-lambda (tm* int) void "systemtime_set_day_of_month"))
(define systemtime-day-of-year (c-lambda (tm*) int "systemtime_day_of_year"))
(define systemtime-set-day-of-year (c-lambda (tm* int) void "systemtime_set_day_of_year"))
(define systemtime-day-of-week (c-lambda (tm*) int "systemtime_day_of_week"))
(define systemtime-set-day-of-week (c-lambda (tm* int) void "systemtime_set_day_of_week"))
(define systemtime-is-dst (c-lambda (tm*) bool "systemtime_is_dst"))
(define systemtime-set-is-dst (c-lambda (tm* bool) void "systemtime_set_is_dst"))

(define-structure host-time tm)

(define (now)
  (make-host-time 
   (seconds-to-systemtime 
    (current-time-seconds))))

(define is_time host-time?)

(define (time_to_seconds t) (systemtime-to-seconds (host-time-tm t)))
(define (seconds_to_time s) 
  (make-host-time
   (seconds-to-systemtime s)))

(define (time_second t) (systemtime-second (host-time-tm t)))
(define (time_set_second t s) (systemtime-set-second (host-time-tm t) s))

(define (time_minute t) (systemtime-minute (host-time-tm t)))
(define (time_set_minute t s) (systemtime-set-minute (host-time-tm t) s))

(define (time_hour t) (systemtime-hour (host-time-tm t)))
(define (time_set_hour t s) (systemtime-set-hour (host-time-tm t) s))

(define (time_day_of_month t) (systemtime-day-of-month (host-time-tm t)))
(define (time_set_day_of_month t s) (systemtime-set-day-of-month (host-time-tm t) s))

(define (time_month t) (systemtime-month (host-time-tm t)))
(define (time_set_month t s) (systemtime-set-month (host-time-tm t) s))

(define (time_year t) (systemtime-year (host-time-tm t)))
(define (time_set_year t s) (systemtime-set-year (host-time-tm t) s))

(define (time_day_of_week t) (systemtime-day-of-week (host-time-tm t)))
(define (time_set_day_of_week t s) (systemtime-set-day-of-week (host-time-tm t) s))

(define (time_day_of_year t) (systemtime-day-of-year (host-time-tm t)))
(define (time_set_day_of_year t s) (systemtime-set-day-of-year (host-time-tm t) s))

(define (time_is_dst t) (systemtime-is-dst (host-time-tm t)))
(define (time_set_is_dst t s) (systemtime-set-is-dst (host-time-tm t) s))

(define (process_times) (f64vector->list (process-times)))
(define cpu_time cpu-time)
(define real_time real-time)
