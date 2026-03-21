# download_file_safe handles errors

    Code
      res <- download_file_safe("http://example.com/fail", tempfile(), show_progress = FALSE)
    Condition
      Warning:
      Error downloading from: http://example.com/fail
      Details: Simulated error
      The resource may be temporarily unavailable.

