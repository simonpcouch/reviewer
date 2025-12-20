// Editor JavaScript functionality

$(document).ready(function() {
  // Scroll to a specific line in the editor
  function scrollToLine(lineNum) {
    const line = $(`.editor-line[data-line="${lineNum}"]`);
    if (line.length > 0) {
      const container = $('.editor-container');
      const containerTop = container.scrollTop();
      const containerHeight = container.height();
      const lineTop = line.position().top + containerTop;

      // Scroll to put the line in the upper third of the viewport
      const targetScroll = lineTop - (containerHeight / 3);
      container.animate({ scrollTop: Math.max(0, targetScroll) }, 300);
    }
  }

  // Auto-scroll the editor to show the editable region
  function scrollToRegion() {
    const regionLine = $('.editor-line.in-region').first();
    if (regionLine.length > 0) {
      const container = $('.editor-container');
      const containerTop = container.scrollTop();
      const containerHeight = container.height();
      const lineTop = regionLine.position().top + containerTop;
      const lineHeight = regionLine.outerHeight();

      // Scroll to put region in upper third
      const targetScroll = lineTop - (containerHeight / 3);
      container.animate({
        scrollTop: Math.max(0, targetScroll)
      }, 300);
    }
  }

  // Handle scroll-to-line custom message
  Shiny.addCustomMessageHandler('scroll-to-line', function(data) {
    scrollToLine(data.line);
  });

  // Scroll to region when the editor updates
  $(document).on('shiny:value', function(event) {
    if (event.name && event.name.includes('file_editor')) {
      setTimeout(scrollToRegion, 100);
    }
  });

  // Initial scroll on page load
  setTimeout(scrollToRegion, 500);
});
