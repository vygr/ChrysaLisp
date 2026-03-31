Update the Molecule demo to use a lazy, none blocking, distributed Local render farm.

Shared constants/structures and functions, between the `child.lisp` and
`app.lisp`, should be placed into a `app.inc` file.

The farm should be created on demand and after a period of inactive job lists,
be closed. Also closed on app exit.

We should track both jobs_qued and jobs_in_flight. Discovery of no available
shared atom cache image, should que a new job. Jobs distributed to a child task
should move to the job_in_flight list. As a job completes it entry can be
discarded.

The Local farm should have controls +max_workers 8, +init_workers_% 10,
+grow_workers_% 10. These % calculation factors don't need max with 1, that is
handled by the Local farm class.

The child worker task is responsible for rendering the requested atom image
cache file. The parent should lazy create the farm (and set the inactivity timer
running), as required when outstanding atom renders are not available.

We should have several timers running, the frame rate of the display, the retry
timer for the farm :refresh, the inactivity check timer. The retry timeout value
can be low as we expect a worker to be able to render a new atom image cache
entry in short order.

If an atom texture entry cannot be found in the cache, the job is qued, if not
already present, and that draw list entry is ignored for this frame.

The result of these updates, should be a demo that uses dynamically rendered
atom image cache files, shared between all instances of the Molecule demo. The
work to render any missing cache entries is lazily done and does not stall
drawing operations.