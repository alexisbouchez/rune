# =============================================================================
# Rune Reactive System ("Signals")
# =============================================================================
# A minimal reactive system with automatic dependency tracking.
#
# Three primitives:
#   signal(initial)   - a mutable reactive value with get()/set()
#   computed(fn)       - a derived value that auto-tracks signal dependencies
#   effect(fn)         - a side-effect that re-runs when dependencies change
#
# Dependency tracking uses a global "tracking context" stack. When a computed
# or effect function is running, any signal$get() call registers that signal
# as a dependency. When a signal$set() fires, it notifies all dependents.
#
# This is the same approach used by SolidJS, Preact Signals, Vue 3 reactivity.
# =============================================================================

# Global tracking stack. Each entry is an "observer" (computed or effect).
# When non-empty, signal$get() registers the current observer as a dependent.
.rune_tracking <- new.env(parent = emptyenv())
.rune_tracking$stack <- list()
.rune_tracking$batch_depth <- 0L
.rune_tracking$pending_effects <- list()

#' Push an observer onto the tracking stack.
push_observer <- function(observer) {
  .rune_tracking$stack <- c(.rune_tracking$stack, list(observer))
}

#' Pop the current observer from the tracking stack.
pop_observer <- function() {
  n <- length(.rune_tracking$stack)
  if (n == 0) return(NULL)
  obs <- .rune_tracking$stack[[n]]
  .rune_tracking$stack <- .rune_tracking$stack[-n]
  obs
}

#' Get the current observer (top of stack), or NULL if not tracking.
current_observer <- function() {
  n <- length(.rune_tracking$stack)
  if (n == 0) return(NULL)
  .rune_tracking$stack[[n]]
}

# =============================================================================
# signal(initial) -> list with get()/set()
# =============================================================================

#' Create a reactive signal.
#' @param initial The initial value.
#' @return A list with $get() and $set(value) methods.
signal <- function(initial) {
  env <- new.env(parent = emptyenv())
  env$value <- initial
  env$observers <- list()  # list of observer environments that depend on this
  env$id <- paste0("sig_", as.integer(as.numeric(Sys.time()) * 1000), "_",
                    sample.int(10000, 1))

  get_fn <- function() {
    # Register dependency if someone is tracking
    obs <- current_observer()
    if (!is.null(obs)) {
      # Add this observer to our observers (if not already there)
      already <- any(vapply(env$observers, function(o) identical(o, obs), logical(1)))
      if (!already) {
        env$observers <- c(env$observers, list(obs))
      }
    }
    env$value
  }

  set_fn <- function(new_value) {
    if (!identical(env$value, new_value)) {
      env$value <- new_value
      notify_observers(env)
    }
    invisible(new_value)
  }

  structure(
    list(get = get_fn, set = set_fn, .env = env),
    class = "rune_signal"
  )
}

#' Notify all observers of a signal that its value changed.
notify_observers <- function(signal_env) {
  # Copy observer list (observers might modify the list during notification)
  obs_list <- signal_env$observers

  for (obs in obs_list) {
    if (is.environment(obs) && !is.null(obs$run)) {
      if (.rune_tracking$batch_depth > 0L) {
        # Batching: defer effect execution
        .rune_tracking$pending_effects <- c(.rune_tracking$pending_effects, list(obs))
      } else {
        obs$run()
      }
    }
  }
}

# =============================================================================
# computed(fn) -> list with get()
# =============================================================================

#' Create a computed (derived) reactive value.
#' @param fn A function that reads signals and returns a value.
#' @return A list with $get() method.
computed <- function(fn) {
  env <- new.env(parent = emptyenv())
  env$fn <- fn
  env$value <- NULL
  env$dirty <- TRUE
  env$observers <- list()  # other computeds/effects that depend on this
  env$id <- paste0("comp_", as.integer(as.numeric(Sys.time()) * 1000), "_",
                    sample.int(10000, 1))

  # The "run" function re-evaluates
  env$run <- function() {
    env$dirty <- TRUE
    # If anyone depends on us, notify them too
    for (obs in env$observers) {
      if (is.environment(obs) && !is.null(obs$run)) {
        obs$run()
      }
    }
  }

  get_fn <- function() {
    # Register dependency from current tracking context
    obs <- current_observer()
    if (!is.null(obs)) {
      already <- any(vapply(env$observers, function(o) identical(o, obs), logical(1)))
      if (!already) {
        env$observers <- c(env$observers, list(obs))
      }
    }

    if (env$dirty) {
      # Track our own dependencies
      push_observer(env)
      env$value <- tryCatch(env$fn(), error = function(e) {
        warning("Rune computed error: ", e$message)
        NULL
      })
      pop_observer()
      env$dirty <- FALSE
    }

    env$value
  }

  # Initial evaluation
  push_observer(env)
  env$value <- env$fn()
  pop_observer()
  env$dirty <- FALSE

  structure(
    list(get = get_fn, .env = env),
    class = "rune_computed"
  )
}

# =============================================================================
# effect(fn)
# =============================================================================

#' Create a reactive effect (side-effect that re-runs on dependency changes).
#' @param fn A function that reads signals and performs side effects.
#' @return Invisible NULL. The effect is registered and runs immediately once.
effect <- function(fn) {
  env <- new.env(parent = emptyenv())
  env$fn <- fn
  env$id <- paste0("eff_", as.integer(as.numeric(Sys.time()) * 1000), "_",
                    sample.int(10000, 1))

  env$run <- function() {
    # Re-run the effect, re-tracking dependencies
    push_observer(env)
    tryCatch(env$fn(), error = function(e) {
      warning("Rune effect error: ", e$message)
    })
    pop_observer()
  }

  # Run immediately to establish dependencies
  env$run()

  invisible(env)
}

# =============================================================================
# batch(fn) - batch multiple signal updates
# =============================================================================

#' Batch multiple signal updates, deferring effect execution until the end.
#' @param fn A function that calls signal$set() one or more times.
batch <- function(fn) {
  .rune_tracking$batch_depth <- .rune_tracking$batch_depth + 1L
  tryCatch({
    fn()
  }, finally = {
    .rune_tracking$batch_depth <- .rune_tracking$batch_depth - 1L
    if (.rune_tracking$batch_depth == 0L) {
      # Flush pending effects (deduplicated)
      pending <- .rune_tracking$pending_effects
      .rune_tracking$pending_effects <- list()
      # Deduplicate by identity
      seen <- list()
      for (obs in pending) {
        already <- any(vapply(seen, function(s) identical(s, obs), logical(1)))
        if (!already) {
          seen <- c(seen, list(obs))
          obs$run()
        }
      }
    }
  })
}
