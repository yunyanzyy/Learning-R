##用R看琅琊榜小说的正确姿势

@(R语言)[琅琊榜|数据分析]

####目录：
零：写在前面的一些废话
一、R眼看琅琊榜的基本原理
1、导入数据
2、筛选数据
3、多条件筛选对话
4、导出数据
二、R眼看琅琊榜的基础分析
1、快速对文本分章节
2、快速定位人物出场章节
3、快速定位人物互动章节
三、总结


####零：写在前面的一些废话

最近电视剧琅琊榜非常之火，除了主角以外，里面很多配角都非常出彩。

原著琅琊榜也是非常精彩的。有些电视剧里没明说的解析，在小说里会明文说出来，这让智商不够的楼主终于看懂了电视剧里各个对话的含义。

不过小说有些太长，作者非常喜欢铺设伏笔以及涉及多个人物，如果想只看一个人或者一个团体的活时，非常困难。

然后楼主偶然间想到了R语言的文本挖掘项目，试着用readLines读入了小说全文，一个新的世界打开了。

综上，本文以一个伪程序员+伪数据分析的角度，讲一下用R语言读琅琊榜的一些故事。

这个也可以用于以后做一些读书笔记和线索整理。

####一、R眼看琅琊榜的基本原理

**导入数据/筛选数据/导出txt或html文件**

#####1、导入数据

首先，我们要从[R语言官方网站](https://www.r-project.org/)下载R语言程序本体。

然后把小说全文储存在一个txt文档里。使用ReadLines读入所有文本。

```{r}
#readLines里面放的是txt地址
text<-readLines("D:/琅琊榜/LangYaBang.txt")
text<-text[nchar(text)!=0]
#预览一下：
text[40:50]
```
![Alt text](./1446253690823.png)

可以看到，用readLines读入txt后，R会帮我们把小说文档按空行分段，生成长达20000多的文本向量，而这个就是我们用来做文本整理的主要材料。

#####2、筛选数据

接下来，我们就可以抓取一些数据来看了。

在写作领域，有一本书叫做《你的剧本逊毙了》，第43个技巧里有提到，要塑造好人物，写好任务的对白时，在A-B对话时，要拼了老命去想A的对话，直到在脑子里听到他的声音，抓住了他的说话特点，再去想B的对话。

而在写作时，保证每一个人的讲话都带着他独有的特点，在塑造人物上非常重要。

所以，要分析琅琊榜小说人物特点，我们可以先试试把人物对话摘出来。

简单分析小说文本，我们会发现在琅琊榜小说里，对话基本都是用双引号“”括起来的。我们只需要用**grep**，就能很轻易把这些对话提取出来，代码示例如下：

```
#方法1：用grepl出TRUE/FALSE，再与直接储存的text对照提取
conversation<-text[grepl("“|”",text)]
#方法2：用grep直接看对话效果
conversation_temp<-grep("“|”",text,value=TRUE)

#取个样本看看
conversation[sample(1:length(conversation),14)]
```

查看一下数据。

![Alt text](./1446254944925.png)

恩，用双引号确实把我们需要的对话都摘了出来，存在了conversation变量里面。

#####3、多条件筛选对话

现在，我们增加一些条件，再继续筛选一下。譬如说，我们看了电视剧，对能让飞流避之不及的蔺晨非常感兴趣，想看看飞流与蔺晨的互动，我们可以用以下代码：

```
feiliu_linchen<-text[grepl("飞流",text)&grepl("蔺晨|阁主|蔺大公子",text)]
#随机选几个样本看看
feiliu_linchen[sample(1:length(feiliu_linchen),20)]
```
![Alt text](./1446255624922.png)

R现在自动帮我们把段落里又出现蔺晨，又出现飞流的片段抓了出来。

一个很有意思的点是，在小说前期蔺晨没有出现时，梅长苏已经多次提到说，飞流，你想不想蔺晨哥哥啊，飞流，你忘了蔺晨哥哥怎么教你的啊，等等等等。虽然飞流已经很听苏哥哥的话了，但是从这些对话里我们会发现在教育飞流方面，蔺晨哥哥还是出现得非常频繁的。就更别提等小说后期蔺晨到金陵以后，他们每天鸡飞狗跳，你追我赶的生活了。

那么，我们可以继续按照各种逻辑，把我们想看的都找出来。

简单来说就是，grepl("条件",要筛选的字符串向量)。

”条件“这里，我们可以用“ | ” 把两个有OR关系的中文字符分隔开，譬如**grep(“靖王|景琰|水牛”,text,value=TRUE)**，会把段落里含有靖王或者景琰或者水牛这样的字样全部摘选出来。

但是一个条件只能有OR的关系。如果要并列查找，则需写多个grepl，譬如

**text[grepl("靖王殿下",text) & grepl("梅长苏",text)]**

如果在grepl前加一个”!"号，则表示去除所有有某个字段的段落，举例如：

**text[(!grepl("靖王",text) )&grepl("景琰",text) & grepl("梅长苏",text) & grepl("“|”",text)]**

这一段，只会给你找出，是对话（看最后一个grepl条件），有”景琰“和”梅长苏“出现，但不出现“靖王”的段落。

![Alt text](./1446259176151.png)

具体的用法可以搜索网上各种“R语言 grepl 正则表达式”等方法。

下面提供一些代码示例。

```
#示例1： 飞流与靖王殿下的所有互动
text[grepl("飞流",text)&grepl("靖王|景琰",text)]

#示例2：飞流与梅长苏的互动对话
conversation[!grepl("梅长苏",conversation) & grepl("飞流",conversation)]

#示例3：梅长苏提及靖王殿下时的对话
text[grepl("靖王殿下",text) & grepl("梅长苏",text)& grepl("“|”",text)]

```

![Alt text](./1446257842102.png)

综上，依据这些，我们就可以把想要的文段摘选出来一一欣赏了。

#####4、导出数据

但是摘选出来还不够，有时候我们会希望可以把它导出到本地盘，像txt或者html那样保存起来。

R提供了write函数，可以导出字符串为txt文件，或者直接生成html。

请看示例：

```
景琰_梅长苏<-text[(!grepl("靖王",text) )&grepl("景琰",text) & grepl("梅长苏",text) & grepl("“|”",text)]
#导出为txt文件：
write(景琰_梅长苏,"D:/琅琊榜/景琰&梅长苏.text")
#接下来，导出html。
#需要把段落与段落之间加上html分行代码，否则导出来的文字会密密麻麻挤在一起
html_1<-paste(景琰_梅长苏,collapse = "<br></br>")
write(html_1,"D:/琅琊榜/景琰&梅长苏.html")

```

这里我存在了D盘琅琊榜文件夹里，运行代码看看，文件已经放在D盘里了~~

这样的结果，就可以简单分享给其他人看。
![Alt text](./1446259271602.png)


####二、R眼看琅琊榜的基础分析

#####1、快速对文本分章节

做完上面这一步，我们来聊聊剧情。

剧情是由人物而带动的。虽然我们看的小说，往往都有章节的内容，但有时候只看标题，真的很难确定这个章节的内容。

我们先对琅琊榜小说按章节分拆。涉及一些其他函数，大家直接看代码吧

```
#grep，用.代替任何一个字符
章节名<-(grep("第.卷",text,value=TRUE))
章节分段<-c(grep("第.卷",text),length(text)+1)
#创建一个文件夹，放分好段的章节
dir.create("D:/琅琊榜/章节分段",recursive=TRUE)
#接下来写一个循环，会把所有章节储存在一个list里，同时生成相对应的txt文件
分段章节<-list()
for (i in 1:(length(章节分段)-1)){
        data<-text[章节分段[i]:章节分段[i+1]-1]
        分段章节[[i]]<-data
        names(分段章节)[i]<-章节名[i]
        write(data,paste0("D:/琅琊榜/章节分段/",i,"-",章节名[i],".txt"))
}
```

打开文件夹看看，所有章节都已经储存起来了。
![Alt text](./1446262525183.png)

然后我们也得到了一个叫做“分段章节”的list,里面储存了所有的分段数据。

#####2、快速定位人物出场章节

好了，源数据处理好，我们可以开始做一下分析了。

这里我们主要用到的是R里面的sapply函数，还有前文所介绍的grepl

关键代码如下：

**sapply(分段章节,function(e) sum(grepl("梅长苏",e)))**


```

#首先，我们读入想要的人物数据。这里可以用前文提到的"|"来指代其他称呼。

角色<-c("梅长苏|梅宗主|苏哲|苏先生|江左梅郎","靖王|景琰|水牛","林殊|小殊",
"飞流","蔺晨|阁主","蒙挚|蒙大统领","霓凰|郡主","景睿",
"豫津|浴巾","穆青|穆小王爷","梁王|皇帝|陛下","静妃|静嫔|静贵妃",
"言侯|言阙","夏冬|冬姐","甄平","黎纲","誉王|景桓","赤焰","列战英|战英","大梁","谢玉|谢侯","滑族","夏江","公主")

#其次，创建用来储存角色信息的文件夹
dir.create("D:/琅琊榜/角色",recursive=TRUE)

#然后写一个循环，用我们刚刚提到的函数，去写一段代码，把统计数据写入角色统计data.frame来。

角色统计<-data.frame(卷名=names(分段章节))
#循环开始
for (a in 1:length(角色)){
角色统计[,a+1]<-sapply(分段章节,function(e) sum(grepl(角色[a],e)))
names(角色统计)[a+1]<-角色[a]
#计算好章节个数后，导出相应文件为HTML
output<-text[grepl(角色[a],text)]
output<-paste(output,collapse = "<br></br>")
filename<-(strsplit(角色[a],"\\|"))[[1]][1]
write(output,paste0("D:/琅琊榜/角色/",filename,".html"))
}
write.csv(角色统计,"D:/琅琊榜/角色统计.csv")
```


好了，运行整段代码，在D:/琅琊榜/角色/这里，我们就能看到把小说拆分成若干个小片段的HTML

![Alt text](./1446265444685.png)

这样子，想要回顾个别人物的情节很方便，然后如果想要看哪个章节出现该人物对话最多，打开琅琊榜目录下的角色统计，就都有了

举个例子，看下图吧，靖王殿下第一次出场在第16章，然后第7章应该是飞流的主场。

![Alt text](./1446265652667.png)

这样子按照出现段落数来找相应的章节，回顾起来就更加方便了！

我们还可以对它做一个联合，譬如说选出“梅长苏”与“靖王”出场次数都很多的章节，如17章，正好就是“麒麟择主”。

各个联合着来看，想看什么人物对决，只要看这张表，也就可以了。

有了这样规整的一张数据表，就是想做一些数据分析啊，画一些人物图啊等等，都非常方便。

#####3、快速定位人物互动章节

下面我们改一改上面的代码，专门来看那些有交锋的关系。请看代码示例：

```
dir.create("D:/琅琊榜/互动with梅长苏/",recursive=TRUE)
互动with梅长苏<-data.frame(卷名=names(分段章节))
for (a in 2:length(角色)){
        互动with梅长苏[,a]<-sapply(分段章节,
                                function(e) sum(
                                        grepl(角色[a],e) & grepl(角色[1],e)))
        names(互动with梅长苏)[a]<-角色[a]
        output<-text[grepl(角色[a],text)& grepl(角色[1],text)]
        output<-paste(output,collapse = "<br></br>")
        filename<-(strsplit(角色[a],"\\|"))[[1]][1]
        write(output,paste0("D:/琅琊榜/互动with梅长苏/梅长苏with",filename,".html"))
}
write.csv(互动with梅长苏,"D:/琅琊榜/互动with梅长苏.csv")
```



####三、总结

用到的R的知识点主要包括：
1、选取数据的方法：grepl/grep
2、统计数据的方法：sapply
3、多条件筛选： | 与&的选择，还有!grepl
4、导出：write输出txt/html的方法
5、循环：for循环

这个方法，不仅仅可以用于琅琊榜，也可以同理推广到其他各种小说文献，甚至用来做课本里的专业术语搜索。

适合理清思路，梳理纹路。

而这一切，只需要我们会写一点点的小R，然后脑洞稍微开大一点，就都可以做到啦。

至于更深层次的，譬如研究人员喜欢做的热词分析，文本挖掘，甚至写作习惯分析，只要有思路，也是可以继续往下做下去的。

不过注意的是，在中文分词方面，似乎R这方面的包没有python来得痛快。如果真的涉及到分词啊什么的，各位可以每种都去看看。

但是对于小说来说，我们抓住人物和场景去厘清线索，也大概就够了。

不说了，看剧去~~~

最后提供一下压缩包给那些不想玩代码的人……
