from gen import *

c = Context.from_root('./site')
print(c)
Alt(
    Seq(
        FilterExt('.md'),
        Map(Pandoc)
    ),
    Seq(
        FilterPath('p'),
        Map(Mako('templates/post.html')),
        Tag('md')
    ),
    # Seq(
    #     FilterExt('.png'),
    #     PrintContext(),
    # ),
    Seq(
        FilterExt(),
        New('posts.html', {'title': 'Posts'}),
        GetTag('md'),
        Map(Mako('templates/post-list.html'))
    ),
    Seq(
        FilterExt('.html'),
        Map(Mako('templates/default.html'))
    ),
).build(c).write('build')