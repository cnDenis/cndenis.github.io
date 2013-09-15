---
layout: page
title: cnDenis的笔记
tagline:
---
{% include JB/setup %}

<ul class="post-list">
  {% for post in site.posts %}
    <li><hr>
        <span class='date'>{{ post.date | date: "%Y-%m-%d" }}</span>&raquo; <h3 class='title'><a href="{{ BASE_PATH }}{{ post.url }}">{{ post.title }}</a></h3>
        <div class='excerpt'>{{ post.excerpt }}</div>
    </li>
  {% endfor %}
</ul>



