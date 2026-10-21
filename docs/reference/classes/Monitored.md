# Monitored

```image
docs/reference/classes/Monitored.cwb
```

## Jobs

```code
(Monitored path task_mbox reply_mbox told [size away]) -> jobs

jobs that say how they go as they go. told is called with the app's
own part of a job, what follows its +job: (told :start job) as it is
given to a child, again if it is given again, its child gone or too
long over it, and (told :done job answer) as its answer comes, the
app's own part of that too, what follows its +job_reply
```

### :answered

```code
(. jobs :answered msg) -> :nil | num
```

### :give

```code
(. jobs :give key val) -> jobs
```

