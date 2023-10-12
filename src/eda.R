library('ProjectTemplate')
load.project()

# Check for missing values -> None
skim(train)

# Check distribution of target ->
train %>%
  select(defects) %>%
  group_by(defects) %>%
  summarise(n = n())

train %>%
  ggplot(aes(x = defects, colour = defects, fill = defects)) +
  geom_bar(show.legend = FALSE) +
  labs(y = 'n', x = 'Defect', title = "Distribution of defects") +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

# Check distribution of features

## loc
train %>%
  ggplot(aes(x = loc)) +
  geom_histogram() +
  xlim(0, 750)

train %>%
  ggplot(aes(
    x = defects,
    y = log(loc),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## v.g.
train %>%
  ggplot(aes(x = v.g.)) +
  geom_histogram() +
  xlim(0, 150)

train %>%
  ggplot(aes(
    x = defects,
    y = log(v.g.),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## ev.g.
train %>%
  ggplot(aes(x = ev.g.)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(ev.g.),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## iv.g.
train %>%
  ggplot(aes(x = iv.g.)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(iv.g.),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## n
train %>%
  ggplot(aes(x = n)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(n + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(n) %>%
  group_by(n == 0) %>%
  summarize(n = n())

## v
train %>%
  ggplot(aes(x = v)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(v + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(v) %>%
  group_by(v == 0) %>%
  summarize(n = n())

## l
train %>%
  ggplot(aes(x = l)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = l,
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## d
train %>%
  ggplot(aes(x = d)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(d),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))


## i
train %>%
  ggplot(aes(x = i)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(i),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))


## e
train %>%
  ggplot(aes(x = e)) +
  geom_histogram()


train %>%
  ggplot(aes(
    x = defects,
    y = log(e),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(e) %>%
  group_by(e == 0) %>%
  summarize(n = n())

## b
train %>%
  ggplot(aes(x = b)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(b + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(b) %>%
  group_by(b == 0) %>%
  summarize(n = n())


## t
train %>%
  ggplot(aes(x = t)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(t + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(t) %>%
  group_by(t == 0) %>%
  summarize(n = n())

## lOCode
train %>%
  ggplot(aes(x = lOCode)) +
  geom_histogram() +
  xlim(0, 1000)

train %>%
  ggplot(aes(
    x = defects,
    y = log(lOCode),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## lOComment
train %>%
  ggplot(aes(x = lOComment)) +
  geom_histogram() +
  xlim(0, 100)

train %>%
  ggplot(aes(
    x = defects,
    y = log(lOComment + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## lOBlank
train %>%
  ggplot(aes(x = lOBlank)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = log(lOBlank + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

## locCodeAndComment
train %>%
  ggplot(aes(x = locCodeAndComment)) +
  geom_histogram()

train %>%
  ggplot(aes(
    x = defects,
    y = locCodeAndComment,
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(locCodeAndComment) %>%
  group_by(locCodeAndComment == 0) %>%
  summarize(n = n())

## uniq_Op
train %>%
  ggplot(aes(x = uniq_Op)) +
  geom_histogram() +
  xlim(0, 100)


train %>%
  ggplot(aes(
    x = defects,
    y = uniq_Op,
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(uniq_Op) %>%
  group_by(uniq_Op == 0) %>%
  summarize(n = n())

## uniq_Opnd
train %>%
  ggplot(aes(x = uniq_Opnd)) +
  geom_histogram() +
  xlim(0, 100)


train %>%
  ggplot(aes(
    x = defects,
    y = log(uniq_Opnd + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(uniq_Opnd) %>%
  group_by(uniq_Opnd == 0) %>%
  summarize(n = n())

## total_Op
train %>%
  ggplot(aes(x = total_Op)) +
  geom_histogram()


train %>%
  ggplot(aes(
    x = defects,
    y = log(total_Op + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(total_Op) %>%
  group_by(total_Op == 0) %>%
  summarize(n = n())

## total_Opnd
train %>%
  ggplot(aes(x = total_Opnd)) +
  geom_histogram()


train %>%
  ggplot(aes(
    x = defects,
    y = log(total_Opnd + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(total_Opnd) %>%
  group_by(total_Opnd == 0) %>%
  summarize(n = n())

## branchCount
train %>%
  ggplot(aes(x = branchCount)) +
  geom_histogram()


train %>%
  ggplot(aes(
    x = defects,
    y = log(branchCount + 1),
    colour = defects,
    fill = defects
  )) +
  geom_boxplot() +
  scale_colour_manual(values = c("darkgreen", "red"),
                      aesthetics = c("colour", "fill"))

train %>%
  select(branchCount) %>%
  group_by(branchCount == 0) %>%
  summarize(n = n())


# Test for normality (sample size is very large)
print(
  train %>%
    select(-defects) %>%
    map(ad.test) %>%
    map(tidy) %>%
    bind_rows(.id = "feature"),
  n = 50
)

# Plot 
qq_plot <- function(vector, name) {
  tibble(name = vector) %>% ggplot(aes(sample = name)) +
    stat_qq() + 
    stat_qq_line() +
    labs(x = name)
}

qq_plots <- train %>%
  select(-defects) %>%
  mutate_all(scale) %>%
  imap(qq_plot) 

do.call("grid.arrange", c(qq_plots, ncol = 4)) 



