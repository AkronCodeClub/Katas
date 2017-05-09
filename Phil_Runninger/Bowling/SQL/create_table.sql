CREATE TABLE [dbo].[bowling](
    [frame] [tinyint] NOT NULL,
    [ball] [tinyint] NOT NULL,
    [pins] [tinyint] NOT NULL,
 CONSTRAINT [PK_bowling] PRIMARY KEY CLUSTERED 
(
    [frame] ASC,
    [ball] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
