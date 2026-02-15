# =============================================================================
# Tests: HTML Node Model
# =============================================================================

cat("=== HTML Tests ===\n")

source(file.path(Sys.getenv("RUNE_ROOT", ".."), "tests", "test_helpers.R"), local = FALSE)
source(file.path(core_dir, "R", "html.R"), local = FALSE)

cat("\nrune_node & render_html:\n")

# Basic tag
n1 <- div("hello")
assert("div creates rune_node", inherits(n1, "rune_node"))
assert("div has correct tag", n1$tag == "div")
assert("div renders correctly", render_html(n1) == "<div>hello</div>")

# Tag with attributes
n2 <- div(class = "foo", id = "bar", "content")
html2 <- render_html(n2)
assert("attributes render", grepl('class="foo"', html2))
assert("id attribute renders", grepl('id="bar"', html2))
assert("content renders", grepl("content", html2))

# Nested tags
n3 <- div(p("inner"))
assert("nested tags render", render_html(n3) == "<div><p>inner</p></div>")

# Void elements
n4 <- br()
assert("void element self-closes", render_html(n4) == "<br />")

n5 <- input(type = "text", value = "hi")
html5 <- render_html(n5)
assert("input is void", grepl("/>", html5))
assert("input has type attr", grepl('type="text"', html5))

# HTML escaping
n6 <- p("<script>alert('xss')</script>")
html6 <- render_html(n6)
assert("text content is escaped", grepl("&lt;script&gt;", html6))
assert("no raw script tag", !grepl("<script>", html6))

# Attribute escaping
n7 <- div(title = 'say "hello"')
html7 <- render_html(n7)
assert("attribute values are escaped", grepl("&quot;", html7))

# Raw HTML
n8 <- raw_html("<b>bold</b>")
assert("raw_html passes through", render_html(n8) == "<b>bold</b>")

# Multiple children
n9 <- div(p("one"), p("two"), p("three"))
assert("multiple children render",
       render_html(n9) == "<div><p>one</p><p>two</p><p>three</p></div>")

# NULL children are skipped
n10 <- div("a", NULL, "b")
assert("NULL children skipped", render_html(n10) == "<div>ab</div>")

# List of children (e.g. from lapply)
items <- lapply(1:3, function(i) li(as.character(i)))
n11 <- ul(items)
assert("list of children renders",
       render_html(n11) == "<ul><li>1</li><li>2</li><li>3</li></ul>")

# Boolean attributes
n12 <- input(type = "checkbox", checked = TRUE, disabled = FALSE)
html12 <- render_html(n12)
assert("boolean TRUE renders as bare attr", grepl("checked", html12))
assert("boolean FALSE omitted", !grepl("disabled", html12))

report("HTML tests")
