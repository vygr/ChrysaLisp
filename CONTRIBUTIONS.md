# ChrysaLisp Contribution Guidelines

Thank you for your interest in contributing to the ChrysaLisp project. To ensure
the stability, portability, and high quality of the codebase, all pull requests
(PRs) must adhere to a strict set of verification standards.

## Before You Contribute

Before writing any code, it is essential that you familiarize yourself with the
project's architecture, philosophies, and coding conventions.

* **Read the Documentation:** The primary source of information is the `docs/`
  folder. Please review its contents thoroughly.

* **Start with the AI Digest:** The documents in the `docs/ai_digest/` folder,
  as detailed in `LLM.md`, provide a comprehensive overview of the core
  concepts. While originally created for AI, they serve as an excellent
  introduction for human developers as well.

## Contribution Requirements

All submitted pull requests must meet the following non-negotiable requirements
to be considered for merging.

1. **Adherence to Coding Style:** Your code must strictly follow the established
   coding style and architectural guidelines found throughout the ChrysaLisp
   codebase. The source code itself provides numerous examples of these
   conventions in practice.

2. **Work from the `master` Branch:** All PRs must be based on and tested
   against the latest `master` branch. Please rebase your work before submission
   to ensure a clean merge.

3. **Functionality and Platform Testing:**

    * You must verify that all existing functionality, particularly the GUI demo
      applications, continues to run correctly after your changes.

    * If your contribution involves changes to the C++ Platform Implementation
      Interface (PII) or any other platform-specific code, you are required to
      test and validate your changes across **all supported platforms**.

4. **Build Verification and Reproducibility:** The ChrysaLisp build system is
   designed to be fully deterministic. Your contributions must uphold this
   standard.

    * **The `make it` Gold Standard:** The `make it` command, when run from
      within the ChrysaLisp TUI or GUI Terminal, is the official test for build
      integrity. It performs a full cross-platform build, generates all boot
      images, and builds the documentation.

    * **`make install` Verification:** Following a successful `make it`, the
      host platform `make snapshot` followed by `make install` command must also
      complete successfully. This step is critical as it generates the system
      fresh from the `snapshot.zip` file, which is the official distribution
      artifact for new releases and the master branch.

    * **Binary Integrity:** Your changes must be shown to **repeatedly build the
      entire system with no binary differences between builds.** After a
      successful `make it` and `make install`, subsequent build cycles must
      still produce bit-for-bit identical files in the `obj/` directory.

    * **Emulator Mode Reproducibility:** This requirement for binary
      reproducibility must also be met when running the build in the **emulated
      VP64 mode** (e.g., via `./run_tui.sh -e`). The only acceptable difference
      is the speed of execution.

Pull requests that do not meet these criteria will not be accepted. These
standards are in place to safeguard the integrity and unique performance
characteristics of the ChrysaLisp operating system.