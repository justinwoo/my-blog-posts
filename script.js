copy(
  Array.from(document.querySelectorAll(".media__body"))
    .map(x => ({
      date: x.querySelector(".ItemLink__info").innerText,
      title: x.querySelector(".u-link-no-underline").innerText,
      url: x.querySelector(".u-link-no-underline").href
    }))
    .map(x => `${x.date.substr(19)} ${x.title} <${x.url}>`)
    .join("\n")
);
