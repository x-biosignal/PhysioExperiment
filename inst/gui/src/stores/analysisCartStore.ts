import { create } from 'zustand'
import { devtools } from 'zustand/middleware'
import {
  Workflow,
  AnalysisMethod,
  analysisMethods,
  workflows
} from '@/lib/workflows'

export type AnalysisStatus = 'pending' | 'running' | 'completed' | 'error'

export interface CartItem {
  id: string
  methodId: string
  method: AnalysisMethod
  params: Record<string, unknown>
  isRequired: boolean
  status: AnalysisStatus
  result?: AnalysisResult
  error?: string
  addedAt: number
}

export interface AnalysisResult {
  type: 'plot' | 'table' | 'metric' | 'matrix' | 'timeseries' | 'synergy' | 'synergyComparison'
  data: unknown
  metadata?: Record<string, unknown>
  executionTime?: number
}

export interface ReportData {
  workflowId: string
  workflowName: string
  datasetName: string
  generatedAt: Date
  sections: ReportSectionData[]
  summary: ReportSummary
}

export interface ReportSectionData {
  id: string
  title: string
  items: CartItem[]
  layout: 'full' | 'half' | 'third' | 'grid'
}

export interface ReportSummary {
  totalMethods: number
  completedMethods: number
  significantFindings: string[]
  keyMetrics: Array<{ label: string; value: string | number; unit?: string }>
}

interface AnalysisCartState {
  // Cart state
  items: CartItem[]
  selectedWorkflow: Workflow | null
  isCartOpen: boolean

  // Execution state
  isRunning: boolean
  currentItemId: string | null
  progress: number

  // Report state
  reportData: ReportData | null
  isReportVisible: boolean

  // Actions
  setWorkflow: (workflowId: string) => void
  addItem: (methodId: string, params?: Record<string, unknown>) => void
  removeItem: (itemId: string) => void
  updateItemParams: (itemId: string, params: Record<string, unknown>) => void
  clearCart: () => void
  toggleCart: () => void

  // Execution actions
  runAnalysis: (executeMethod: (item: CartItem) => Promise<AnalysisResult>) => Promise<void>
  setItemStatus: (itemId: string, status: AnalysisStatus, result?: AnalysisResult, error?: string) => void

  // Report actions
  generateReport: (datasetName: string) => void
  showReport: () => void
  hideReport: () => void
  exportReport: (format: 'html' | 'pdf' | 'json') => void
}

export const useAnalysisCartStore = create<AnalysisCartState>()(
  devtools(
    (set, get) => ({
      items: [],
      selectedWorkflow: null,
      isCartOpen: false,
      isRunning: false,
      currentItemId: null,
      progress: 0,
      reportData: null,
      isReportVisible: false,

      setWorkflow: (workflowId: string) => {
        const workflow = workflows[workflowId]
        if (!workflow) return

        // Clear existing items
        set({ items: [], selectedWorkflow: workflow })

        // Add required methods with default params
        const requiredSteps = workflow.steps.filter(s => s.required)
        const newItems: CartItem[] = []

        requiredSteps.forEach(step => {
          const method = analysisMethods[step.methodId]
          if (method) {
            const defaultParams: Record<string, unknown> = {}
            method.params.forEach(p => {
              defaultParams[p.name] = step.defaultParams?.[p.name] ?? p.default
            })

            newItems.push({
              id: `${step.methodId}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
              methodId: step.methodId,
              method,
              params: defaultParams,
              isRequired: true,
              status: 'pending',
              addedAt: Date.now()
            })
          }
        })

        set({ items: newItems })
      },

      addItem: (methodId: string, params?: Record<string, unknown>) => {
        const method = analysisMethods[methodId]
        if (!method) return

        // Check if already in cart
        const existing = get().items.find(i => i.methodId === methodId)
        if (existing) return

        const defaultParams: Record<string, unknown> = {}
        method.params.forEach(p => {
          defaultParams[p.name] = p.default
        })

        const newItem: CartItem = {
          id: `${methodId}-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
          methodId,
          method,
          params: { ...defaultParams, ...params },
          isRequired: false,
          status: 'pending',
          addedAt: Date.now()
        }

        set(state => ({
          items: [...state.items, newItem].sort((a, b) => {
            // Sort by workflow order if workflow is selected
            const workflow = state.selectedWorkflow
            if (workflow) {
              const orderA = workflow.steps.find(s => s.methodId === a.methodId)?.order ?? 999
              const orderB = workflow.steps.find(s => s.methodId === b.methodId)?.order ?? 999
              return orderA - orderB
            }
            return a.addedAt - b.addedAt
          })
        }))
      },

      removeItem: (itemId: string) => {
        const item = get().items.find(i => i.id === itemId)
        if (item?.isRequired) return // Can't remove required items

        set(state => ({
          items: state.items.filter(i => i.id !== itemId)
        }))
      },

      updateItemParams: (itemId: string, params: Record<string, unknown>) => {
        set(state => ({
          items: state.items.map(item =>
            item.id === itemId
              ? { ...item, params: { ...item.params, ...params } }
              : item
          )
        }))
      },

      clearCart: () => {
        set({
          items: [],
          selectedWorkflow: null,
          reportData: null,
          isReportVisible: false
        })
      },

      toggleCart: () => {
        set(state => ({ isCartOpen: !state.isCartOpen }))
      },

      runAnalysis: async (executeMethod) => {
        const items = get().items
        if (items.length === 0) return

        set({ isRunning: true, progress: 0 })

        for (let i = 0; i < items.length; i++) {
          const item = items[i]
          set({ currentItemId: item.id })

          // Update status to running
          get().setItemStatus(item.id, 'running')

          try {
            const result = await executeMethod(item)
            get().setItemStatus(item.id, 'completed', result)
          } catch (error) {
            get().setItemStatus(item.id, 'error', undefined, String(error))
          }

          set({ progress: ((i + 1) / items.length) * 100 })
        }

        set({ isRunning: false, currentItemId: null, progress: 100 })
      },

      setItemStatus: (itemId, status, result, error) => {
        set(state => ({
          items: state.items.map(item =>
            item.id === itemId
              ? { ...item, status, result, error }
              : item
          )
        }))
      },

      generateReport: (datasetName: string) => {
        const { items, selectedWorkflow } = get()
        if (!selectedWorkflow) return

        const completedItems = items.filter(i => i.status === 'completed')

        // Group items by report sections
        const sections: ReportSectionData[] = selectedWorkflow.reportSections.map(section => ({
          id: section.id,
          title: section.titleJa,
          items: completedItems.filter(item =>
            section.methodIds.length === 0 || section.methodIds.includes(item.methodId)
          ),
          layout: section.layout
        }))

        // Generate summary
        const significantFindings: string[] = []
        const keyMetrics: Array<{ label: string; value: string | number; unit?: string }> = []

        // Extract significant findings from results
        completedItems.forEach(item => {
          if (item.result?.metadata) {
            const meta = item.result.metadata as Record<string, unknown>
            if (meta.significant) {
              significantFindings.push(`${item.method.nameJa}: 有意な結果`)
            }
            if (meta.pValue !== undefined) {
              keyMetrics.push({
                label: `${item.method.nameJa} p値`,
                value: (meta.pValue as number).toFixed(4)
              })
            }
            if (meta.effectSize !== undefined) {
              keyMetrics.push({
                label: '効果量 (d)',
                value: (meta.effectSize as number).toFixed(3)
              })
            }
          }
        })

        const reportData: ReportData = {
          workflowId: selectedWorkflow.id,
          workflowName: selectedWorkflow.nameJa,
          datasetName,
          generatedAt: new Date(),
          sections,
          summary: {
            totalMethods: items.length,
            completedMethods: completedItems.length,
            significantFindings,
            keyMetrics
          }
        }

        set({ reportData, isReportVisible: true })
      },

      showReport: () => set({ isReportVisible: true }),
      hideReport: () => set({ isReportVisible: false }),

      exportReport: (format) => {
        const { reportData } = get()
        if (!reportData) return

        if (format === 'json') {
          const dataStr = JSON.stringify(reportData, null, 2)
          const blob = new Blob([dataStr], { type: 'application/json' })
          const url = URL.createObjectURL(blob)
          const a = document.createElement('a')
          a.href = url
          a.download = `report-${reportData.datasetName}-${Date.now()}.json`
          a.click()
          URL.revokeObjectURL(url)
        }
        // HTML and PDF export would require additional implementation
      }
    }),
    { name: 'analysis-cart-store' }
  )
)
