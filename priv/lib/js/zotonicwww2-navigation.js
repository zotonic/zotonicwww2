(function () {
    "use strict";

    const HIDDEN_CLASS = "site-nav-hidden";
    const DIRECTION_DISTANCE = 10;

    function scrollPosition() {
        return Math.max(0, window.scrollY || document.documentElement.scrollTop || 0);
    }

    function initNavigation() {
        const header = document.getElementById("header-wrapper");

        if (!header) {
            return;
        }

        let previousPosition = scrollPosition();
        let directionPosition = previousPosition;
        let direction = 0;
        let isScheduled = false;

        function showNavigation() {
            document.body.classList.remove(HIDDEN_CLASS);
        }

        function hideNavigation() {
            const isSearchOpen = document.body.classList.contains("search-overlay-open");
            const hasHeaderFocus = header.contains(document.activeElement);

            if (isSearchOpen || hasHeaderFocus) {
                showNavigation();
            } else {
                document.body.classList.add(HIDDEN_CLASS);
            }
        }

        function updateNavigation() {
            const position = scrollPosition();
            const delta = position - previousPosition;
            const newDirection = delta === 0 ? direction : Math.sign(delta);

            isScheduled = false;

            if (newDirection !== direction) {
                direction = newDirection;
                directionPosition = previousPosition;
            }

            if (position <= header.offsetHeight) {
                showNavigation();
            } else if (Math.abs(position - directionPosition) >= DIRECTION_DISTANCE) {
                if (direction < 0) {
                    showNavigation();
                } else if (direction > 0) {
                    hideNavigation();
                }
            }

            previousPosition = position;
        }

        function scheduleUpdate() {
            if (!isScheduled) {
                isScheduled = true;
                window.requestAnimationFrame(updateNavigation);
            }
        }

        function resetNavigation() {
            previousPosition = scrollPosition();
            directionPosition = previousPosition;
            direction = 0;
            showNavigation();
        }

        window.addEventListener("scroll", scheduleUpdate, { passive: true });
        window.addEventListener("resize", resetNavigation);
        window.addEventListener("pageshow", resetNavigation);
        document.addEventListener("focus", function (event) {
            if (header.contains(event.target)) {
                showNavigation();
            }
        }, true);

        resetNavigation();
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initNavigation);
    } else {
        initNavigation();
    }
}());
