Unhack is an "in-code" issue and technical debt manager. it helps developers creating issues without leaving their text editor.

**Why?**

Developers live writing code in their text editors; using other tools outside of it can be quite a distraction. It's pretty common to find issues while working on something. A bug that you discovered, an idea for a feature, a task that you don't want to forget, or even some contextual notes that you want to make sure are tracked. Having to leave the text editor, go to your project management tool and fill in a bunch of forms? The reality is that nobody does that. The result is that many issues are left without registering them anywhere, gone forever. In projects that teams adopted the Unhack system, the difference was massive; hundreds of issues were registered by developers in the code.

Traditionally, developers resorted to the use of @todo annotations. Unhack builds on top of that idea allowing you to add issue types, labels, priority and notes. It then statically scans Git repositories for such annotations and stores the results in a data store (Elastic Search by default). All registered issues are then available in a decoupled web application. Teams can then review all issues for each repository, and track progress on every commit.

Unhack therefore functions as an additional, powerful tool for managing technical debt.
