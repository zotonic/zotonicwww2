(function () {
    "use strict";

    function searchForm(id) {
        return id ? document.getElementById(id) : null;
    }

    function updateFacet(link) {
        var form = searchForm(link.dataset.searchForm);
        var input = form && form.elements.namedItem(link.dataset.searchFacet);

        if (!input) {
            return false;
        }

        input.value = input.value === link.dataset.searchValue ? "" : link.dataset.searchValue;
        form.dispatchEvent(new Event("change", { bubbles: true }));
        return true;
    }

    function clearFacets(link) {
        var form = searchForm(link.dataset.searchForm);

        if (!form) {
            return false;
        }

        ["category", "subject", "module"].forEach(function (name) {
            var input = form.elements.namedItem(name);
            if (input) {
                input.value = "";
            }
        });
        form.dispatchEvent(new Event("change", { bubbles: true }));
        return true;
    }

    function initSearchOverlay() {
        var overlay = document.getElementById("site-search-overlay");
        var form = document.getElementById("site-search-form");
        var input = document.getElementById("site-search");
        var header = document.getElementById("header-wrapper");

        if (!overlay || !form || !input) {
            return;
        }

        function positionOverlay() {
            if (!header) {
                return;
            }

            overlay.style.setProperty(
                "--site-search-top",
                Math.max(0, Math.round(header.getBoundingClientRect().bottom)) + "px"
            );
        }

        function openOverlay() {
            positionOverlay();
            overlay.hidden = false;
            input.setAttribute("aria-expanded", "true");
            document.body.classList.add("search-overlay-open");
        }

        function closeOverlay() {
            overlay.hidden = true;
            input.setAttribute("aria-expanded", "false");
            document.body.classList.remove("search-overlay-open");
        }

        input.addEventListener("focus", openOverlay);
        input.addEventListener("input", openOverlay);
        input.addEventListener("search", function () {
            form.dispatchEvent(new Event("change", { bubbles: true }));
        });

        window.addEventListener("resize", function () {
            if (!overlay.hidden) {
                positionOverlay();
            }
        });

        if (window.visualViewport) {
            window.visualViewport.addEventListener("resize", function () {
                if (!overlay.hidden) {
                    positionOverlay();
                }
            });
        }

        document.addEventListener("click", function (event) {
            var facet = event.target.closest("[data-search-facet]");
            var clear = event.target.closest("[data-search-clear]");
            var close = event.target.closest("[data-search-close]");

            if (facet && updateFacet(facet)) {
                event.preventDefault();
            } else if (clear && clearFacets(clear)) {
                event.preventDefault();
            } else if (close) {
                closeOverlay();
            }
        });

        document.addEventListener("keydown", function (event) {
            if (event.key === "Escape" && !overlay.hidden) {
                closeOverlay();
                input.blur();
            }
        });
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initSearchOverlay);
    } else {
        initSearchOverlay();
    }
}());
