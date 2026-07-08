// Custom transform toggle — handle clicks on .tfm-btn elements.
// Updates visual state and pushes value to Shiny reactives via setInputValue.

document.addEventListener("click", function (e) {
  var btn = e.target.closest(".tfm-btn");
  if (!btn) return;

  var inputId = btn.getAttribute("data-tfm-input");
  var value   = btn.getAttribute("data-tfm-value");

  // Swap active class among siblings
  btn.closest(".tfm-group").querySelectorAll(".tfm-btn").forEach(function (b) {
    b.classList.remove("tfm-active");
  });
  btn.classList.add("tfm-active");

  // Update the Shiny reactive input
  Shiny.setInputValue(inputId, value, { priority: "event" });
});
