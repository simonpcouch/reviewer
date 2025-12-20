// Reviewer JavaScript - handles approval flow

$(document).ready(function() {
  // Handle Accept button click
  $(document).on('click', '.btn-accept', function(e) {
    e.preventDefault();
    const card = $(this).closest('.comment-card');
    const proposalId = card.data('proposal-id');

    // Send approval to Shiny
    Shiny.setInputValue('edit_response', {
      request_id: proposalId,
      approved: true,
      feedback: null
    }, { priority: 'event' });

    // Animate card removal
    card.addClass('removing');
    setTimeout(function() {
      card.remove();
    }, 200);
  });

  // Handle Reject button click
  $(document).on('click', '.btn-reject', function(e) {
    e.preventDefault();
    const card = $(this).closest('.comment-card');
    const proposalId = card.data('proposal-id');
    const feedbackInput = card.find('.comment-feedback input');
    const feedback = feedbackInput.val().trim() || null;

    // Send rejection to Shiny
    Shiny.setInputValue('edit_response', {
      request_id: proposalId,
      approved: false,
      feedback: feedback
    }, { priority: 'event' });

    // Animate card removal
    card.addClass('removing');
    setTimeout(function() {
      card.remove();
    }, 200);
  });

  // Handle Enter key in feedback input to submit feedback
  $(document).on('keypress', '.comment-feedback input', function(e) {
    if (e.which === 13) { // Enter key
      e.preventDefault();
      const card = $(this).closest('.comment-card');
      const proposalId = card.data('proposal-id');
      const feedback = $(this).val().trim();

      if (feedback) {
        // Send feedback as rejection with message
        Shiny.setInputValue('edit_response', {
          request_id: proposalId,
          approved: false,
          feedback: feedback
        }, { priority: 'event' });

        // Animate card removal
        card.addClass('removing');
        setTimeout(function() {
          card.remove();
        }, 200);
      }
    }
  });

  // Focus feedback input when clicking in the card (but not on buttons)
  $(document).on('click', '.comment-card', function(e) {
    if (!$(e.target).closest('.comment-actions').length) {
      $(this).find('.comment-feedback input').focus();
    }
  });
});
