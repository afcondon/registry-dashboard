// FFI for CompilerMatrix click handlers
export const addGroupClickHandlers = containerSelector => callback => () => {
  // Use event delegation on the container
  const container = document.querySelector(containerSelector);
  if (!container) return;

  // Remove any existing handler to avoid duplicates
  container._matrixClickHandler && container.removeEventListener('click', container._matrixClickHandler);

  // Create new handler
  container._matrixClickHandler = (event) => {
    // Find the group header that was clicked
    let target = event.target;
    while (target && target !== container) {
      if (target.classList && target.classList.contains('group-header')) {
        const major = target.getAttribute('data-major');
        if (major) {
          callback(major)();
        }
        return;
      }
      target = target.parentElement;
    }
  };

  container.addEventListener('click', container._matrixClickHandler);
};
