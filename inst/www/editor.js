// Editor JavaScript functionality

$(document).ready(function() {
  // Configure highlight.js for R syntax
  if (window.hljs) {
    hljs.configure({ languages: ['r'] });
  }

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

      // Scroll to put region in upper third
      const targetScroll = lineTop - (containerHeight / 3);
      container.animate({
        scrollTop: Math.max(0, targetScroll)
      }, 300);
    }
  }

  // Apply syntax highlighting to each line
  function highlightLines() {
    if (!window.hljs) return;

    $('.line-content').each(function() {
      const $line = $(this);
      const $parent = $line.closest('.editor-line');

      // Skip diff lines - they have special markup we don't want to destroy
      if ($parent.hasClass('diff-changed') ||
          $parent.hasClass('diff-removed') ||
          $parent.hasClass('diff-added')) {
        return;
      }

      $line.addClass('hljs');
      const existingRaw = $line.data('raw');
      const rawText = existingRaw !== undefined ? existingRaw : $line.text();

      // Preserve blank lines without injecting markup
      if (!rawText.trim()) {
        $line.data('raw', rawText);
        $line.text(rawText);
        return;
      }

      let result;
      try {
        result = hljs.highlight(rawText, { language: 'r', ignoreIllegals: true });
      } catch (e) {
        result = hljs.highlightAuto(rawText);
      }

      $line.data('raw', rawText);
      $line.html((result && result.value) ? result.value : rawText);
    });
  }

  function enhanceEditor() {
    setTimeout(function() {
      highlightLines();
      scrollToRegion();
    }, 120);
  }

  // Handle scroll-to-line custom message
  Shiny.addCustomMessageHandler('scroll-to-line', function(data) {
    scrollToLine(data.line);
  });

  // Enhance when the editor updates
  $(document).on('shiny:value', function(event) {
    if (event.name && event.name.includes('file_editor')) {
      enhanceEditor();
    }
  });

  // Initial enhancement on page load
  setTimeout(enhanceEditor, 500);
});
