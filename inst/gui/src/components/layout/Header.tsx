import { useAppStore } from '@/stores/appStore'
import { useDataStore } from '@/stores/dataStore'
import {
  Activity,
  Sun,
  Moon,
  Monitor,
  Search,
  Bell,
  HelpCircle,
} from 'lucide-react'
import { cn } from '@/lib/utils'

export function Header() {
  const { theme, setTheme, isConnected } = useAppStore()
  const activeDataset = useDataStore((state) =>
    state.activeDatasetId ? state.datasets[state.activeDatasetId] : null
  )

  const cycleTheme = () => {
    const themes: Array<'light' | 'dark' | 'system'> = ['light', 'dark', 'system']
    const currentIndex = themes.indexOf(theme)
    const nextIndex = (currentIndex + 1) % themes.length
    setTheme(themes[nextIndex])
  }

  const ThemeIcon = theme === 'light' ? Sun : theme === 'dark' ? Moon : Monitor

  return (
    <header className="sticky top-0 z-40 flex h-14 items-center justify-between border-b bg-card px-4">
      {/* Logo & Title */}
      <div className="flex items-center gap-3">
        <div className="flex h-8 w-8 items-center justify-center rounded-lg bg-primary">
          <Activity className="h-5 w-5 text-primary-foreground" />
        </div>
        <div>
          <h1 className="text-lg font-semibold">PhysioExperiment</h1>
          {activeDataset && (
            <p className="text-xs text-muted-foreground">
              {activeDataset.name} - {activeDataset.nChannels} ch, {activeDataset.samplingRate} Hz
            </p>
          )}
        </div>
      </div>

      {/* Search */}
      <div className="flex-1 max-w-md mx-8">
        <div className="relative">
          <Search className="absolute left-3 top-1/2 h-4 w-4 -translate-y-1/2 text-muted-foreground" />
          <input
            type="text"
            placeholder="Search commands, datasets..."
            className="w-full rounded-lg border bg-background py-2 pl-10 pr-4 text-sm placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-primary"
          />
          <kbd className="absolute right-3 top-1/2 -translate-y-1/2 rounded bg-muted px-1.5 py-0.5 text-xs text-muted-foreground">
            ⌘K
          </kbd>
        </div>
      </div>

      {/* Actions */}
      <div className="flex items-center gap-2">
        {/* Connection Status */}
        <div
          className={cn(
            'flex items-center gap-2 rounded-full px-3 py-1 text-xs',
            isConnected
              ? 'bg-green-500/10 text-green-600 dark:text-green-400'
              : 'bg-red-500/10 text-red-600 dark:text-red-400'
          )}
        >
          <span
            className={cn(
              'h-2 w-2 rounded-full',
              isConnected ? 'bg-green-500' : 'bg-red-500'
            )}
          />
          {isConnected ? 'Connected' : 'Disconnected'}
        </div>

        {/* Theme Toggle */}
        <button
          onClick={cycleTheme}
          className="rounded-lg p-2 text-muted-foreground hover:bg-accent hover:text-accent-foreground"
          title={`Theme: ${theme}`}
        >
          <ThemeIcon className="h-5 w-5" />
        </button>

        {/* Notifications */}
        <button className="relative rounded-lg p-2 text-muted-foreground hover:bg-accent hover:text-accent-foreground">
          <Bell className="h-5 w-5" />
          <span className="absolute right-1 top-1 h-2 w-2 rounded-full bg-primary" />
        </button>

        {/* Help */}
        <button className="rounded-lg p-2 text-muted-foreground hover:bg-accent hover:text-accent-foreground">
          <HelpCircle className="h-5 w-5" />
        </button>
      </div>
    </header>
  )
}
