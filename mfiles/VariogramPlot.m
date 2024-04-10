function VariogramPlot(title)

outputDir = 'Results/';

flnm = [outputDir, 'DIntV.txt'];
x = load(flnm);
flnm = [outputDir, 'GammaIntV.txt'];
y1 = load(flnm);
flnm = [outputDir, 'SIntV.txt'];
y2=load(flnm);

len = size(x,1);

figure('Name', [title, '_', int2str(len)]);
plot(x,y1, x, y2)
xlabel('DIntV')
legend('GammaIntV','SIntV')
grid

saveas(gcf,[outputDir, title '.pdf'])