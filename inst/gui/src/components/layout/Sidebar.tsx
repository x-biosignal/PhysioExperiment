import { Link, useLocation } from 'react-router-dom'
import { useAppStore } from '@/stores/appStore'
import { cn } from '@/lib/utils'
import {
  LayoutDashboard,
  Database,
  Activity,
  LineChart,
  GitBranch,
  Settings,
  ChevronLeft,
  ChevronRight,
  Upload,
  Filter,
  Waves,
  Network,
  BarChart3,
  Folder,
  Workflow,
} from 'lucide-react'

interface NavItem {
  label: string
  icon: React.ComponentType<{ className?: string }>
  href: string
  children?: NavItem[]
}

const navItems: NavItem[] = [
  {
    label: 'Dashboard',
    icon: LayoutDashboard,
    href: '/',
  },
  {
    label: 'Data',
    icon: Database,
    href: '/data',
    children: [
      { label: 'Import', icon: Upload, href: '/data/import' },
      { label: 'Browser', icon: Folder, href: '/data/browser' },
    ],
  },
  {
    label: 'Preprocessing',
    icon: Filter,
    href: '/preprocessing',
    children: [
      { label: 'Filtering', icon: Waves, href: '/preprocessing/filter' },
      { label: 'Artifacts', icon: Activity, href: '/preprocessing/artifacts' },
    ],
  },
  {
    label: 'Analysis',
    icon: LineChart,
    href: '/analysis',
    children: [
      { label: 'Time-Frequency', icon: Waves, href: '/analysis/timefreq' },
      { label: 'Connectivity', icon: Network, href: '/analysis/connectivity' },
      { label: 'Statistics', icon: BarChart3, href: '/analysis/statistics' },
    ],
  },
  {
    label: 'Workflow Analysis',
    icon: Workflow,
    href: '/workflow-analysis',
  },
  {
    label: 'Visualization',
    icon: Activity,
    href: '/visualization',
  },
  {
    label: 'Workflow Builder',
    icon: GitBranch,
    href: '/workflow',
  },
  {
    label: 'Settings',
    icon: Settings,
    href: '/settings',
  },
]

export function Sidebar() {
  const location = useLocation()
  const { sidebarCollapsed, toggleSidebar } = useAppStore()

  return (
    <aside
      className={cn(
        'fixed left-0 top-14 z-30 flex h-[calc(100vh-3.5rem-1.5rem)] flex-col border-r bg-card transition-all duration-300',
        sidebarCollapsed ? 'w-16' : 'w-64'
      )}
    >
      {/* Navigation */}
      <nav className="flex-1 space-y-1 overflow-y-auto p-2">
        {navItems.map((item) => (
          <NavLink
            key={item.href}
            item={item}
            isActive={location.pathname === item.href}
            collapsed={sidebarCollapsed}
          />
        ))}
      </nav>

      {/* Collapse Toggle */}
      <div className="border-t p-2">
        <button
          onClick={toggleSidebar}
          className="flex w-full items-center justify-center rounded-md p-2 text-muted-foreground hover:bg-accent hover:text-accent-foreground"
        >
          {sidebarCollapsed ? (
            <ChevronRight className="h-5 w-5" />
          ) : (
            <>
              <ChevronLeft className="h-5 w-5" />
              <span className="ml-2 text-sm">Collapse</span>
            </>
          )}
        </button>
      </div>
    </aside>
  )
}

interface NavLinkProps {
  item: NavItem
  isActive: boolean
  collapsed: boolean
}

function NavLink({ item, isActive, collapsed }: NavLinkProps) {
  const Icon = item.icon

  return (
    <Link
      to={item.href}
      className={cn(
        'flex items-center rounded-md px-3 py-2 text-sm font-medium transition-colors',
        isActive
          ? 'bg-primary text-primary-foreground'
          : 'text-muted-foreground hover:bg-accent hover:text-accent-foreground',
        collapsed && 'justify-center px-2'
      )}
      title={collapsed ? item.label : undefined}
    >
      <Icon className={cn('h-5 w-5', !collapsed && 'mr-3')} />
      {!collapsed && <span>{item.label}</span>}
    </Link>
  )
}
