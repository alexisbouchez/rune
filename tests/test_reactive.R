# =============================================================================
# Tests: Reactive Signals
# =============================================================================

cat("=== Reactive Tests ===\n")

source(file.path(Sys.getenv("RUNE_ROOT", ".."), "tests", "test_helpers.R"), local = FALSE)
source(file.path(core_dir, "R", "reactive.R"), local = FALSE)

# --- Test signal ---

cat("\nsignal:\n")

s1 <- signal(0L)
assert("signal initial value", s1$get() == 0L)

s1$set(5L)
assert("signal set updates value", s1$get() == 5L)

s1$set(5L)
assert("signal set with same value is no-op", s1$get() == 5L)

s2 <- signal("hello")
assert("signal works with strings", s2$get() == "hello")
s2$set("world")
assert("signal set string", s2$get() == "world")

s3 <- signal(NULL)
assert("signal works with NULL", is.null(s3$get()))
s3$set(42)
assert("signal set from NULL", s3$get() == 42)

# --- Test computed ---

cat("\ncomputed:\n")

s4 <- signal(3L)
c1 <- computed(function() s4$get() * 2L)
assert("computed initial value", c1$get() == 6L)

s4$set(10L)
assert("computed updates when signal changes", c1$get() == 20L)

# Chained computed
s5 <- signal(2L)
c2 <- computed(function() s5$get() + 1L)
c3 <- computed(function() c2$get() * 10L)
assert("chained computed: c2 = 3", c2$get() == 3L)
assert("chained computed: c3 = 30", c3$get() == 30L)

s5$set(5L)
assert("chained computed after update: c2 = 6", c2$get() == 6L)
assert("chained computed after update: c3 = 60", c3$get() == 60L)

# --- Test effect ---

cat("\neffect:\n")

s6 <- signal(0L)
effect_log <- c()

effect(function() {
  val <- s6$get()
  effect_log <<- c(effect_log, val)
})

assert("effect runs immediately", length(effect_log) == 1L)
assert("effect captured initial value", effect_log[1] == 0L)

s6$set(1L)
assert("effect runs on set", length(effect_log) == 2L)
assert("effect captured new value", effect_log[2] == 1L)

s6$set(2L)
assert("effect runs again", length(effect_log) == 3L)
assert("effect captured value 2", effect_log[3] == 2L)

# Effect with multiple signal dependencies
s7 <- signal("a")
s8 <- signal("b")
combined <- ""

effect(function() {
  combined <<- paste0(s7$get(), s8$get())
})
assert("multi-dep effect initial", combined == "ab")

s7$set("x")
assert("multi-dep effect after s7 change", combined == "xb")

s8$set("y")
assert("multi-dep effect after s8 change", combined == "xy")

# --- Test batch ---

cat("\nbatch:\n")

s9 <- signal(0L)
batch_log <- c()

effect(function() {
  batch_log <<- c(batch_log, s9$get())
})
batch_log <- c()  # reset

batch(function() {
  s9$set(1L)
  s9$set(2L)
  s9$set(3L)
})

assert("batch: effect ran once after batch", length(batch_log) == 1L)
assert("batch: effect got final value", batch_log[1] == 3L)

# --- Test dependency tracking isolation ---

cat("\ndependency tracking:\n")

s10 <- signal(1L)
s11 <- signal(100L)

track_log <- c()

effect(function() {
  val <- s10$get()
  track_log <<- c(track_log, paste0("s10:", val))
})
track_log <- c()  # reset

s11$set(200L)
assert("changing unrelated signal does not trigger effect",
       length(track_log) == 0L)

s10$set(2L)
assert("changing tracked signal triggers effect",
       length(track_log) == 1L)

report("Reactive tests")
