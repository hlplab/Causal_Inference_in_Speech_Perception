###

setwd("/Users/shawncummings/Documents/GitHub/Causal_Inference_in_Speech_Perception/")


s <- read_tsv("./materials/Annotated/s_segments.txt")
sh <- read_tsv("./materials/Annotated/sh_segments.txt")

# dataframe including just CoG for now
d <- rbind(s, sh) %>%
  filter(segment %in% c("S", "SH")) %>%
  mutate(type = ifelse(grepl("50", source) == T, "shifted", "typical"),
         cog = as.numeric(cog)) %>%
  select(segment, word, cog, type)

# quick sanity check
d %>%
  group_by(segment, type) %>%
  summarise(mean = mean(cog),
            sd = sd(cog))

p1 <- d %>%
  ggplot(aes(x = cog,
             fill = segment)) +
  geom_histogram(bins = 20) +
  facet_wrap(~type)
p1

# as expected, typical SH is lowest, then both shifted, and typical S is highest.





