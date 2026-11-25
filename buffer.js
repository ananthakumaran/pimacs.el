(() => {
  const root = document.querySelector('.top-level-extent');
  if (!root) return;

  const isChangelog = root.dataset.buffer === 'changelog';
  const selector = '.chapter-level-extent, .section-level-extent, .subsection-level-extent, .subsubsection-level-extent, .level2, .level3, .level4';
  const initiallyOpen = isChangelog
    ? Array.from(root.querySelectorAll('.level2')).slice(0, 2).map(section => section.id)
    : ['Introduction', 'Setup'];
  root.querySelector('h1')?.classList.add('top');
  const folds = [];
  const titles = new Map();

  for (const section of root.querySelectorAll(selector)) {
    const heading = section.querySelector(':scope > h2, :scope > h3, :scope > h4, :scope > h5');
    if (!heading) continue;

    const title = heading.cloneNode(true);
    title.querySelectorAll('.copiable-link').forEach(link => link.remove());
    const label = title.textContent.trim();
    const details = document.createElement('details');
    const summary = document.createElement('summary');
    const content = document.createElement('div');
    details.className = 'buffer-section';
    content.className = 'buffer-section-body';
    details.open = !section.matches('.chapter-level-extent, .level2') || initiallyOpen.includes(section.id);
    summary.append(heading);
    details.append(summary, content);
    content.append(...section.childNodes);
    section.append(details);
    titles.set(details, label);
    folds.push(details);
  }

  const header = document.createElement('header');
  header.className = 'buffer-header';
  header.innerHTML = `
    <a class="buffer-name" href="index.html">*pimacs:manual*</a>
    <span class="buffer-path" aria-label="Current section">${isChangelog ? 'Changelog' : 'Top'}</span>
    <nav aria-label="Documentation"><a href="changelog.html">changelog</a><a class="buffer-source" href="https://github.com/ananthakumaran/pimacs.el">source</a></nav>
    <select class="buffer-theme" aria-label="Color theme">
      <option value="system" selected>system</option>
      <option value="light">light</option>
      <option value="dark">dark</option>
    </select>`;

  const modeLine = document.createElement('footer');
  modeLine.className = 'buffer-mode-line';
  modeLine.innerHTML = `
    <span class="buffer-readonly" title="Read-only buffer">-:--</span>
    <a class="buffer-name" href="#Top">*pimacs:${isChangelog ? 'changelog' : 'manual'}*</a>
    <span class="buffer-position">Top</span>
    <span class="buffer-key-hint">n/p move · RET fold · ←/→ tree</span>
    <span class="buffer-fold-controls"><button type="button" data-fold="open" aria-label="Expand all sections">expand<span class="buffer-control-all"> all</span></button><button type="button" data-fold="close" aria-label="Collapse all sections">collapse<span class="buffer-control-all"> all</span></button></span>`;

  document.body.prepend(header);
  document.body.append(modeLine);
  document.documentElement.classList.add('buffer-ui');
  root.setAttribute('role', 'main');
  document.documentElement.lang = 'en';

  const path = header.querySelector('.buffer-path');
  const position = modeLine.querySelector('.buffer-position');
  const theme = header.querySelector('.buffer-theme');
  theme.addEventListener('change', () => {
    if (theme.value === 'system') delete document.documentElement.dataset.theme;
    else document.documentElement.dataset.theme = theme.value;
  });

  function updateModeLine() {
    let current;
    for (const fold of folds) {
      const summary = fold.firstElementChild;
      if (summary.getClientRects().length && summary.getBoundingClientRect().top <= header.offsetHeight + 50) current = fold;
    }
    const labels = [];
    for (let fold = current; fold; fold = fold.parentElement.closest('details.buffer-section')) {
      labels.unshift(titles.get(fold));
    }
    if (isChangelog) labels.unshift('Changelog');
    path.textContent = labels.join(' › ') || 'Top';
    const max = document.documentElement.scrollHeight - window.innerHeight;
    position.textContent = window.scrollY < 2 ? 'Top' : max - window.scrollY < 2 ? 'Bot' : `${Math.round(window.scrollY / max * 100)}%`;
  }

  let pending = false;
  function scheduleUpdate() {
    if (pending) return;
    pending = true;
    requestAnimationFrame(() => {
      pending = false;
      updateModeLine();
    });
  }

  for (const fold of folds) fold.addEventListener('toggle', scheduleUpdate);
  window.addEventListener('scroll', scheduleUpdate, { passive: true });
  window.addEventListener('resize', scheduleUpdate);
  window.addEventListener('load', scheduleUpdate);

  modeLine.addEventListener('click', event => {
    const button = event.target.closest('[data-fold]');
    if (!button) return;
    for (const fold of folds) fold.open = button.dataset.fold === 'open';
    scheduleUpdate();
  });

  document.addEventListener('keydown', event => {
    const target = event.target instanceof Element ? event.target : null;
    if (event.defaultPrevented || event.isComposing || event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) return;
    if (target?.closest('input, textarea, select, [contenteditable]:not([contenteditable="false"]), [role="textbox"]')) return;

    if (event.key === 'n' || event.key === 'p') {
      const summaries = folds
        .filter(fold => !fold.parentElement.closest('details:not([open])'))
        .map(fold => fold.firstElementChild);
      if (!summaries.length) return;
      event.preventDefault();
      let index = summaries.indexOf(target?.closest('details.buffer-section')?.firstElementChild);
      if (index === -1) {
        summaries.forEach((summary, i) => {
          if (summary.getBoundingClientRect().top <= header.offsetHeight + 50) index = i;
        });
      }
      const next = summaries[Math.max(0, Math.min(summaries.length - 1, index + (event.key === 'n' ? 1 : -1)))];
      next.focus({ preventScroll: true });
      next.scrollIntoView({ block: 'nearest' });
      scheduleUpdate();
      return;
    }

    if (target?.tagName !== 'SUMMARY' || !root.contains(target)) return;
    const fold = event.target.parentElement;
    if (event.key === 'ArrowRight') {
      event.preventDefault();
      if (!fold.open) fold.open = true;
      else fold.querySelector('.buffer-section-body summary')?.focus();
    } else if (event.key === 'ArrowLeft') {
      event.preventDefault();
      if (fold.open) fold.open = false;
      else fold.parentElement.closest('details')?.firstElementChild.focus();
    }
  });

  function revealHash() {
    let id;
    try {
      id = decodeURIComponent(window.location.hash.slice(1));
    } catch {
      return;
    }
    const target = id && document.getElementById(id);
    if (!target) return;
    let fold = target.matches(selector) ? target.querySelector(':scope > details') : target.closest('details');
    while (fold) {
      fold.open = true;
      fold = fold.parentElement.closest('details');
    }
    requestAnimationFrame(() => {
      target.scrollIntoView({ block: 'start' });
      scheduleUpdate();
    });
  }

  window.addEventListener('hashchange', revealHash);
  root.addEventListener('click', event => {
    const link = event.target.closest('a[href^="#"]');
    if (link && link.hash === window.location.hash) revealHash();
  });
  window.addEventListener('beforeprint', () => {
    for (const fold of folds) {
      fold.dataset.printOpen = String(fold.open);
      fold.open = true;
    }
  });
  window.addEventListener('afterprint', () => {
    for (const fold of folds) fold.open = fold.dataset.printOpen === 'true';
  });

  revealHash();
  scheduleUpdate();
})();
